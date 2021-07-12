{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Web.Eved.Server
    where

import           Control.Applicative  ((<|>))
import           Control.Exception    (Exception, SomeException (..), catch,
                                       handle, throwIO)
import           Control.Monad
import qualified Data.ByteString.Lazy as LBS
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

import qualified Web.Eved.ContentType as CT
import           Web.Eved.Internal
import qualified Web.Eved.QueryParam  as QP
import qualified Web.Eved.UrlElement  as UE


import           Network.Wai          (Application, lazyRequestBody, pathInfo,
                                       queryString, requestHeaders,
                                       requestMethod, responseLBS)

data RequestData a
  = BodyRequestData (LBS.ByteString -> Either Text a)
  | PureRequestData a
  deriving Functor

newtype EvedServerT m a = EvedServerT
    { unEvedServerT :: (forall a. m a -> IO a) -> [Text] -> IO (RequestData a) -> (SomeException -> ServerError) -> Application }


server :: (forall a. m a -> IO a) -> a -> EvedServerT m a -> Application
server nt handlers api req resp =
    unEvedServerT api nt (pathInfo req) (pure (PureRequestData handlers)) defaultErrorHandler req resp
        `catch` (\PathError -> resp $ responseLBS notFound404 [] "Not Found")
        `catch` (\(CaptureError err) -> resp $ responseLBS badRequest400 [] (LBS.fromStrict $ encodeUtf8 err))
        `catch` (\(QueryParamParseError err) -> resp $ responseLBS badRequest400 [] (LBS.fromStrict $ encodeUtf8 err))
        `catch` (\NoContentMatchError -> resp $ responseLBS unsupportedMediaType415 [] "Unsupported Media Type")
        `catch` (\NoAcceptMatchError -> resp $ responseLBS notAcceptable406 [] "Not Acceptable")
        `catch` (\NoMethodMatchError -> resp $ responseLBS methodNotAllowed405 [] "Method Not Allowed")

data PathError = PathError
    deriving Show
newtype CaptureError = CaptureError Text
    deriving Show
data NoContentMatchError = NoContentMatchError
    deriving Show
newtype QueryParamParseError = QueryParamParseError Text
    deriving Show
data NoAcceptMatchError = NoAcceptMatchError
    deriving Show
data NoMethodMatchError = NoMethodMatchError
    deriving Show

instance Exception PathError
instance Exception CaptureError
instance Exception NoContentMatchError
instance Exception QueryParamParseError
instance Exception NoAcceptMatchError
instance Exception NoMethodMatchError

instance Eved (EvedServerT m) m where
    a .<|> b = EvedServerT $ \nt path requestData errorHandler req resp -> do
        let applicationA = unEvedServerT a nt path (fmap (fmap (\(l :<|> _) -> l)) requestData)
            applicationB = unEvedServerT b nt path (fmap (fmap (\(_ :<|> r) -> r)) requestData)
        applicationA errorHandler req resp <|> applicationB errorHandler req resp

    lit s next = EvedServerT $ \nt path action ->
        case path of
            x:rest | x == s -> unEvedServerT next nt rest action
            _               -> \_ _ _ -> throwIO PathError
    capture _s el next = EvedServerT $ \nt path action ->
        case path of
          x:rest ->
              case UE.fromUrlPiece el x of
                Right arg ->
                    unEvedServerT next nt rest $ fmap (fmap ($ arg)) action
                Left err -> \_ _ _ -> throwIO $ CaptureError err
          _ -> \_ _ _ -> throwIO PathError

    reqBody ctypes next = EvedServerT $ \nt path action errHandler req -> do
        let mContentTypeBS = lookup hContentType $ requestHeaders req
        case mContentTypeBS of
              Just contentTypeBS ->
                case CT.chooseContentCType ctypes contentTypeBS of
                      Just bodyParser ->
                          unEvedServerT next nt path (fmap (addBodyParser bodyParser) action) errHandler req
                      Nothing ->
                            \_ -> throwIO NoContentMatchError
              Nothing ->
                  unEvedServerT next nt path (fmap (addBodyParser (CT.fromContentType (NE.head ctypes))) action) errHandler req

       where
            addBodyParser bodyParser action =
                case action of
                  BodyRequestData fn -> BodyRequestData $ \bodyText -> fn bodyText <*> bodyParser bodyText
                  PureRequestData v -> BodyRequestData $ \bodyText -> v <$> bodyParser bodyText

    queryParam s qp next = EvedServerT $ \nt path action errHandler req ->
       let queryText = queryToQueryText (queryString req)
           params = fromMaybe "true" . snd <$> filter (\(k, _) -> k == s) queryText
       in case QP.fromQueryParam qp params of
            Right a  -> unEvedServerT next nt path (fmap (fmap ($a)) action) errHandler req
            Left err -> \_ -> throwIO $ QueryParamParseError err

    verb method status ctypes = EvedServerT $ \nt path action errorHandler req resp ->
        case path of
            [] -> do
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

                -- We are now committed since you can only read the body once
                eResponseData <- handle (pure . Left) $
                                 handle (\e@(SomeException err) -> pure $ Left $ errorHandler e) $
                                 action >>= (\case
                                              BodyRequestData fn -> do
                                                  reqBody <- lazyRequestBody req
                                                  case fn reqBody of
                                                    Right res -> Right <$> nt res
                                                    Left err  -> pure $ Left $ ServerError
                                                            { errorStatus = badRequest400
                                                            , errorBody = LBS.fromStrict $ encodeUtf8 err
                                                            , errorHeaders = []
                                                            }
                                              PureRequestData a ->
                                                  Right <$> nt a)

                case eResponseData of
                  Right responseData ->
                    resp $ responseLBS status [(hContentType, renderHeader ctype)] (renderContent responseData)
                  Left err ->
                    resp $ responseLBS (errorStatus err) (errorHeaders err) (errorBody err)
            _ ->
                throwIO PathError

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
