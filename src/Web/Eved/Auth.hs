{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Web.Eved.Auth
    where

import qualified Data.ByteString     as BS
import           Data.List.NonEmpty  (NonEmpty (..), nonEmpty)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types  (hAuthorization, unauthorized401)
import qualified Network.Wai         as Wai
import qualified Web.Eved.Client     as Client
import           Web.Eved.Internal
import qualified Web.Eved.Server     as Server

auth :: (Eved api m, EvedAuth api, Applicative f)
     => NonEmpty (f (AuthScheme a)) -> f (api b) -> f (api (a -> b))
auth schemes next = auth_ <$> sequenceA schemes <*> next


class EvedAuth api where
    auth_ :: NonEmpty (AuthScheme a) -> api b -> api (a -> b)

data AuthResult a
    = AuthSuccess a
    | AuthFailure
    | AuthNeeded

data AuthScheme a = AuthScheme
    { authenticateRequest :: Wai.Request -> AuthResult a
    , addCredentials      :: a -> HTTP.Request -> HTTP.Request
    }

data BasicAuth = BasicAuth
    { basicAuthUsername :: Text
    , basicAuthPassword :: Text
    }

basicAuth :: AuthScheme BasicAuth
basicAuth = AuthScheme
    { authenticateRequest = \req ->
        case lookup hAuthorization $ Wai.requestHeaders req of
          Just authHeader ->
            let (authType, rest) = T.breakOn " " $ decodeUtf8 authHeader
            in
            if T.toLower authType == "basic" then
               let (username, rest') = T.breakOn ":" $ T.strip rest
                   password = T.drop 1 rest'
               in AuthSuccess (BasicAuth username password)
            else
               AuthNeeded
          Nothing ->
              AuthNeeded
    , addCredentials = \creds ->
        HTTP.applyBasicAuth
            (encodeUtf8 $ basicAuthUsername creds)
            (encodeUtf8 $ basicAuthPassword creds)
    }


instance EvedAuth Client.EvedClient where
    auth_ (scheme :| _) next = Client.EvedClient $ \req a ->
        Client.client next $ addCredentials scheme a req

instance EvedAuth (Server.EvedServerT m) where
    auth_ schemes next = Server.EvedServerT $ \nt path action errorHandler req resp ->
        case go req schemes of
              AuthSuccess a -> Server.unEvedServerT next nt path (fmap (fmap ($ a)) action) errorHandler req resp
              _             -> resp $ Wai.responseLBS unauthorized401 [] "Method Not Allowed"


         where
             go request (s :| rest) =
                 case authenticateRequest s request of
                   AuthSuccess a -> AuthSuccess a
                   AuthFailure -> AuthFailure
                   AuthNeeded -> maybe AuthFailure (go request) (nonEmpty rest)
