{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Web.Eved.Server
    where

import           Control.Applicative  ((<*))
import           Control.Exception    (Exception, SomeException (..), catch,
                                       handle, throwIO, try)
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as List
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Network.HTTP.Media   (renderHeader)
import           Network.HTTP.Types   (Header, Status, badRequest400, hAccept,
                                       hContentType, internalServerError500,
                                       methodNotAllowed405, notAcceptable406,
                                       notFound404, queryToQueryText,
                                       renderStdMethod, unsupportedMediaType415)

import qualified Data.CaseInsensitive as CI
import qualified Web.Eved.ContentType as CT
import qualified Web.Eved.Header      as H
import           Web.Eved.Internal
import qualified Web.Eved.QueryParam  as QP
import qualified Web.Eved.UrlElement  as UE


import           Network.Wai          (Application, Response, lazyRequestBody,
                                       pathInfo, queryString, requestHeaders,
                                       requestMethod, responseLBS)

data RequestData a
  = BodyRequestData (LBS.ByteString -> Either Text a)
  | PureRequestData a
  deriving Functor

newtype EvedServerT m a = EvedServerT
    { unEvedServerT :: (forall a. m a -> IO a) -> [Text] -> RequestData a -> Application }

simpleServer :: a -> EvedServerT IO a -> Application
simpleServer =
    server id

server :: (forall a. m a -> IO a)
       -> a
       -> EvedServerT m a
       -> Application
server = hoistServerWithErrorHandler defaultErrorHandler

hoistServerWithErrorHandler
        :: (SomeException -> ServerError)
        -> (forall a. m a -> IO a)
        -> a
        -> EvedServerT m a
        -> Application
hoistServerWithErrorHandler errorHandler nt handlers api req resp =
    unEvedServerT api nt (List.dropWhileEnd (== "") (pathInfo req)) (PureRequestData handlers) req resp
        `catch` (\case
            PathError -> resp $ responseLBS notFound404 [] "Not Found"
            CaptureError err -> resp $ responseLBS badRequest400 [] (LBS.fromStrict $ encodeUtf8 err)
            QueryParamParseError err -> resp $ responseLBS badRequest400 [] (LBS.fromStrict $ encodeUtf8 err)
            NoContentMatchError -> resp $ responseLBS unsupportedMediaType415 [] "Unsupported Media Type"
            NoAcceptMatchError -> resp $ responseLBS notAcceptable406 [] "Not Acceptable"
            NoMethodMatchError -> resp $ responseLBS methodNotAllowed405 [] "Method Not Allowed")
        `catch` (resp . serverErrorToResponse)
        `catch` (\(UserApplicationError err) -> resp $ serverErrorToResponse err)
        `catch` (\(UserApplicationError err) -> resp $ serverErrorToResponse $ errorHandler err)

data RoutingError
    = PathError
    | CaptureError Text
    | HeaderParseError Text
    | QueryParamParseError Text
    | NoContentMatchError
    | NoAcceptMatchError
    | NoMethodMatchError
    deriving (Show, Eq, Ord)

newtype UserApplicationError a = UserApplicationError a
    deriving Show

instance Exception RoutingError
instance Exception a => Exception (UserApplicationError a)

instance Eved (EvedServerT m) m where
    type UrlElement (EvedServerT m) = UE.FromUrlElement
    type ContentType (EvedServerT m) = CT.ContentType
    type QueryParam (EvedServerT m) = QP.FromQueryParam

    a .<|> b = EvedServerT $ \nt path requestData req resp -> do
        let applicationA = unEvedServerT a nt path (fmap (\(l :<|> _) -> l) requestData)
            applicationB = unEvedServerT b nt path (fmap (\(_ :<|> r) -> r) requestData)
        eApplicationAResult <- try @RoutingError $ applicationA req resp
        case eApplicationAResult of
          Right a -> pure a
          Left errA -> do
            eApplicationBResult <- try @RoutingError $ applicationB req resp
            case eApplicationBResult of
              Right b   -> pure b
              Left errB -> if errA > errB then throwIO errA else throwIO errB

    lit s next = EvedServerT $ \nt path action ->
        case path of
            x:rest | x == s -> unEvedServerT next nt rest action
            _               -> \_ _ -> throwIO PathError

    capture _s el next = EvedServerT $ \nt path action ->
        case path of
          x:rest ->
              case UE.fromUrlPiece el x of
                Right arg ->
                    unEvedServerT next nt rest $ fmap ($ arg) action
                Left err -> \_ _ -> throwIO $ CaptureError err
          _ -> \_ _ -> throwIO PathError

    reqBody ctypes next = EvedServerT $ \nt path action req -> do
        let mContentTypeBS = lookup hContentType $ requestHeaders req
        case mContentTypeBS of
              Just contentTypeBS ->
                case CT.chooseContentCType ctypes mempty contentTypeBS of
                      Just bodyParser ->
                          unEvedServerT next nt path (addBodyParser bodyParser action) req
                      Nothing ->
                            \_ -> throwIO NoContentMatchError
              Nothing ->
                  unEvedServerT next nt path (addBodyParser (CT.fromContentType (NE.head ctypes) . (mempty,)) action) req

       where
            addBodyParser bodyParser action =
                case action of
                  BodyRequestData fn -> BodyRequestData $ \bodyText -> fn bodyText <*> bodyParser bodyText
                  PureRequestData v -> BodyRequestData (fmap v . bodyParser)

    queryParam s qp next = EvedServerT $ \nt path action req ->
       let queryText = queryToQueryText (queryString req)
           params = fromMaybe "true" . snd <$> filter (\(k, _) -> k == s) queryText
       in case QP.fromQueryParam qp params of
            Right a  -> unEvedServerT next nt path (fmap ($a) action) req
            Left err -> \_ -> throwIO $ QueryParamParseError err

    header headerName h next = EvedServerT $ \nt path action req ->
       let ciHeaderName = CI.mk (encodeUtf8 headerName)
           mHeader = lookup ciHeaderName (requestHeaders req)
       in case H.fromHeaderValue h mHeader of
          Right a  -> unEvedServerT next nt path (fmap ($ a) action) req
          Left err -> \_ -> throwIO $ HeaderParseError err

    verb method status ctypes = EvedServerT $ \nt path action req resp -> do
        unless (null path) $
            throwIO PathError

        unless (renderStdMethod method == requestMethod req) $
            throwIO NoMethodMatchError

        (ctype, renderContent) <-
                case lookup hAccept $ requestHeaders req of
                      Just acceptBS ->
                        case CT.chooseAcceptCType ctypes acceptBS of
                          Just (ctype, renderContent) -> pure (ctype, renderContent)
                          Nothing -> throwIO NoAcceptMatchError
                      Nothing ->
                          let ctype = NE.head ctypes
                          in pure (NE.head $ CT.mediaTypes ctype, CT.toContentType ctype)

        (rHeaders, rBody) <-
                    renderContent <$>
                        case action of
                            BodyRequestData fn -> do
                                reqBody <- lazyRequestBody req
                                case fn reqBody of
                                    Right res ->
                                        nt res
                                    Left err ->
                                        throwIO $ ServerError
                                          { errorStatus = badRequest400
                                          , errorBody = LBS.fromStrict $ encodeUtf8 err
                                          , errorHeaders = []
                                          }
                            PureRequestData a ->
                                nt a
               `catch` (\(SomeException e) -> throwIO $ UserApplicationError e)

        resp $ responseLBS status ((hContentType, renderHeader ctype):rHeaders) rBody

serverErrorToResponse :: ServerError -> Response
serverErrorToResponse err =
    responseLBS (errorStatus err) (errorHeaders err) (errorBody err)

data ServerError = ServerError
    { errorStatus  :: Status
    , errorBody    :: LBS.ByteString
    , errorHeaders :: [Header]
    } deriving Show
instance Exception ServerError

defaultErrorHandler :: SomeException -> ServerError
defaultErrorHandler _ =
    ServerError
        { errorStatus = internalServerError500
        , errorBody = "Internal Server Error"
        , errorHeaders = []
        }
