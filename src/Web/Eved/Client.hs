{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Eved.Client
    where

import           Control.Monad.Reader
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (encodeUtf8)
import qualified Network.HTTP.Client  as HttpClient
import           Network.HTTP.Types   (hContentType, parseQuery,
                                       queryTextToQuery, queryToQueryText,
                                       renderQuery, renderStdMethod)
import qualified Web.Eved.ContentType as CT
import           Web.Eved.Internal
import qualified Web.Eved.QueryParam  as QP
import qualified Web.Eved.UrlElement  as UE
import qualified Web.HttpApiData      as HttpApiData

newtype ClientM a = ClientM { unClientM :: ReaderT HttpClient.Manager IO a }

runClientIO :: ClientM a -> IO a
runClientIO m = do
    HttpClient.newManager HttpClient.defaultManagerSettings
        >>= runReaderT (runClient m)

runClient :: (MonadIO m, MonadReader env m, HttpClient.HasHttpManager env) => ClientM a -> m a
runClient (ClientM m) =
    asks HttpClient.getHttpManager >>= (liftIO . runReaderT m)

newtype EvedClient a = EvedClient
    { client :: HttpClient.Request -> a
    }

getClient :: EvedClient a -> Text -> a
getClient (EvedClient f) = f . HttpClient.parseRequest_ . T.unpack

instance Eved EvedClient ClientM where
    l .<|> r = EvedClient $ \req ->
        client l req :<|> client r req

    lit s next = EvedClient $ \req ->
        client next req{ HttpClient.path = HttpClient.path req <> (encodeUtf8 $ HttpApiData.toUrlPiece s) <> "/"}
    capture s el next = EvedClient $ \req a ->
        client next req{ HttpClient.path = HttpClient.path req <> (encodeUtf8 $ UE.toUrlPiece el a) <> "/" }
    reqBody (ctype:|_) next = EvedClient $ \req a ->
        client next req{ HttpClient.requestBody = HttpClient.RequestBodyLBS (CT.toContentType ctype a)
                       , HttpClient.requestHeaders = (CT.contentTypeHeader ctype):HttpClient.requestHeaders req
                       }
    queryParam argName el next = EvedClient $ \req val ->
        client next req{HttpClient.queryString =
            let query = parseQuery $ HttpClient.queryString req
                queryText = queryToQueryText query
                newArgs = fmap (\v -> (HttpApiData.toUrlPiece argName, Just v)) $ QP.toQueryParam el val
            in renderQuery False $ queryTextToQuery (newArgs <> queryText)}

    verb method _status ctypes = EvedClient $ \req -> ClientM $ do
        let reqWithMethod = req{ HttpClient.method = renderStdMethod method
                               , HttpClient.requestHeaders = (CT.acceptHeader ctypes):(HttpClient.requestHeaders req)
                               }
        manager <- ask
        resp <- liftIO $ HttpClient.httpLbs reqWithMethod manager
        let mBodyParser = CT.chooseContentCType ctypes =<< (lookup hContentType $ HttpClient.responseHeaders resp)
        case mBodyParser of
          Just bodyParser  -> case bodyParser (HttpClient.responseBody resp) of
                                Just a -> pure a
                                Nothing -> error "Unimplemented: Content-Type matched but parse failed"
          Nothing -> error "Unimplemented: No Matching Content-Type"

