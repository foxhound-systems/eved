{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Web.Eved.Server
    where

import           Control.Applicative    ((<|>))
import           Control.Exception      (Exception, catch, throwIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Functor           ((<&>))
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as TL
import           Data.Void              (Void, absurd)
import           Network.HTTP.Media     (renderHeader)
import           Network.HTTP.Types     (badRequest400, hAccept, hContentType,
                                         notAcceptable406, queryToQueryText,
                                         renderStdMethod,
                                         unsupportedMediaType415)
import qualified Web.Eved.ContentType   as CT
import           Web.Eved.Internal
import qualified Web.Eved.QueryParam    as QP
import qualified Web.Eved.UrlElement    as UE
import qualified Web.Scotty             as Scotty


import           Network.Wai            (Application, lazyRequestBody, pathInfo,
                                         queryString, requestHeaders,
                                         requestMethod, responseLBS)

data RequestData a
  = BodyRequestData (ByteString -> Either Text a)
  | PureRequestData a
  deriving Functor

newtype EvedServerT m a = EvedServerT
    { unEvedServerT :: (forall a. m a -> IO a) -> [Text] -> IO (RequestData a) -> Application }

serverApplication :: (forall a. m a -> IO a) -> a -> EvedServerT m a -> Application
serverApplication nt handlers api req =
    unEvedServerT api nt (pathInfo req) (pure (PureRequestData handlers)) req

data PathError = PathError
    deriving Show
newtype CaptureError = CaptureError Text
    deriving Show
data NoContentMatchError = NoContentMatchError
    deriving Show
newtype QueryParamParseError = QueryParamParseError Text
    deriving Show
data PathNotExhaustedError = PathNotExhaustedError
    deriving Show
data NoAcceptMatchError = NoAcceptMatchError
    deriving Show
data NoMethodMatchError = NoMethodMatchError
    deriving Show

instance Exception PathError
instance Exception CaptureError
instance Exception NoContentMatchError
instance Exception QueryParamParseError
instance Exception PathNotExhaustedError
instance Exception NoAcceptMatchError
instance Exception NoMethodMatchError

instance Eved (EvedServerT m) m where
    a .<|> b = EvedServerT $ \nt path requestData req resp -> do
        let applicationA = unEvedServerT a nt path (fmap (fmap (\(l :<|> _) -> l)) requestData)
            applicationB = unEvedServerT b nt path (fmap (fmap (\(_ :<|> r) -> r)) requestData)
        applicationA req resp <|> applicationB req resp

    lit s next = EvedServerT $ \nt path action ->
        case path of
            x:rest | x == s -> unEvedServerT next nt rest action
            _               -> \_ _ -> throwIO PathError
    capture _s el next = EvedServerT $ \nt path action ->
        case path of
          x:rest ->
              case UE.fromUrlPiece el x of
                Right arg ->
                    unEvedServerT next nt rest $ fmap (fmap ($ arg)) action
                Left err -> \_ _ -> throwIO $ CaptureError err
          _ -> \_ _ -> throwIO PathError

    reqBody ctypes next = EvedServerT $ \nt path action req -> do
        let mContentTypeBS = lookup hContentType $ requestHeaders req
        case mContentTypeBS of
              Just contentTypeBS ->
                case CT.chooseContentCType ctypes contentTypeBS of
                      Just bodyParser ->
                          unEvedServerT next nt path (fmap (addBodyParser bodyParser) action) req
                      Nothing ->
                            \_ -> throwIO NoContentMatchError
              Nothing ->
                  unEvedServerT next nt path (fmap (addBodyParser (CT.fromContentType (NE.head ctypes))) action) req

       where
            addBodyParser bodyParser action =
                case action of
                  BodyRequestData fn -> BodyRequestData $ \bodyText -> fn bodyText <*> bodyParser bodyText
                  PureRequestData v -> BodyRequestData $ \bodyText -> v <$> bodyParser bodyText

    queryParam s qp next = EvedServerT $ \nt path action req ->
       let queryText = queryToQueryText (queryString req)
           params = fromMaybe "true" . snd <$> filter (\(k, _) -> k == s) queryText
       in case QP.fromQueryParam qp params of
            Right a  -> unEvedServerT next nt path (fmap (fmap ($a)) action) req
            Left err -> \_ -> throwIO $ QueryParamParseError err

    verb method status ctypes = EvedServerT $ \nt path action req resp ->
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
                eResponseData <- action >>= \case
                                  BodyRequestData fn -> do
                                      reqBody <- lazyRequestBody req
                                      case fn reqBody of
                                        Right res -> Right <$> nt res
                                        Left err  -> pure $ Left err
                                  PureRequestData a ->
                                      Right <$> nt a

                case eResponseData of
                  Right responseData ->
                    resp $ responseLBS status [(hContentType, renderHeader ctype)] (renderContent responseData)
                  Left err ->
                    resp $ responseLBS badRequest400 [] (LBS.fromStrict $ encodeUtf8 err)
            _ ->
                throwIO PathNotExhaustedError

newtype EvedScottyT m a = EvedScottyT
    { unEvedScottyT :: (forall a. m a -> IO a) -> Text -> Scotty.ActionM a -> Scotty.ScottyM ()
    }

scottyServer :: (forall a. m a -> IO a) -> EvedScottyT m a -> a -> Scotty.ScottyM ()
scottyServer nt scottyEved handlers =
    unEvedScottyT scottyEved (liftIO . nt) mempty (pure handlers)

instance Eved (EvedScottyT m) m where
    a .<|> b = EvedScottyT $ \nt r action ->
        let left = unEvedScottyT a nt r (do (f :<|> _) <- action; pure f)
            right = unEvedScottyT b nt r (do (_ :<|> g) <- action; pure g)
        in left <> right

    lit s next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt (r <> "/" <> s) action

    capture s el next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt (r <> "/:" <> s) $ do
            mArg <- fmap (UE.fromUrlPiece el) $ Scotty.param $ TL.fromStrict s
            case mArg of
              Right arg -> fmap ($ arg) action
              Left _    -> Scotty.next

    reqBody ctypes next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt r $ do
            contentTypeH <- Scotty.header "Content-Type"
            let contentTypeBS = maybe "" (encodeUtf8 . TL.toStrict) contentTypeH
            case CT.chooseContentCType ctypes contentTypeBS of
              Just bodyParser  -> do
                  requestBody <- Scotty.body
                  case bodyParser requestBody of
                      Right a ->
                          fmap ($ a) action
                      Left err ->
                          Scotty.raiseStatus badRequest400 (TL.fromStrict err)
              Nothing ->
                  Scotty.raiseStatus unsupportedMediaType415 ""

    queryParam s qp next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt r $ do
            ps <- Scotty.params <&> filter (\(k, _) -> k == TL.fromStrict s)
            let vals = fmap (TL.toStrict . snd) ps
            case QP.fromQueryParam qp vals of
              Right v  -> fmap ($ v) action
              Left err -> Scotty.next

    verb method status ctypes = EvedScottyT $ \nt r action ->
        Scotty.addroute method (fromString $ T.unpack r) $ do
            liftIO . print =<< Scotty.request
            acceptH <- Scotty.header "Accept"
            let acceptBS = maybe "" (encodeUtf8 . TL.toStrict) acceptH
            case CT.chooseAcceptCType ctypes acceptBS of
              Just (ctype, renderContent) -> do
                  Scotty.setHeader "Content-Type" (TL.fromStrict $ decodeUtf8 $ renderHeader ctype)
                  Scotty.status status
                  action >>= liftIO . nt
                         >>= Scotty.raw . renderContent
              Nothing ->
                  Scotty.raiseStatus notAcceptable406 mempty
