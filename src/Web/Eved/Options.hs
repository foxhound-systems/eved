{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Web.Eved.Options
    where

import qualified Data.ByteString    as B
import           Data.Coerce        (coerce)
import           Data.Text          (Text)
import           Network.HTTP.Types
import           Network.Wai
import           Web.Eved.Internal

provideOptions :: EvedOptions m a -> Middleware
provideOptions api app req respond
  | requestMethod req == "OPTIONS" = respond (getOptionsResponse api req)
  | otherwise = app req respond

getOptionsResponse :: EvedOptions m a -> Request -> Response
getOptionsResponse api req =
    let methods = renderStdMethod <$> getAvailableMethods api (pathInfo req)
        headers = [ ("Allow", B.intercalate ", " $ "OPTIONS":methods) ]
    in responseBuilder status200 headers mempty

newtype EvedOptions (m :: * -> *) a = EvedOptions
    { getAvailableMethods :: [Text] -> [StdMethod]
    }

passthrough :: EvedOptions m a -> EvedOptions m b
passthrough = coerce

instance Eved (EvedOptions m) m where
    left .<|> right = EvedOptions $ \path ->
        getAvailableMethods left path <> getAvailableMethods right path

    lit t next = EvedOptions $ \case
        p:rest | p == t -> getAvailableMethods next rest
        _               -> mempty

    capture _ _ next = EvedOptions $ \case
        _:rest -> getAvailableMethods next rest
        _      -> mempty

    reqBody _ = passthrough
    queryParam _ _ = passthrough
    header _ _ = passthrough
    verb method _ _ = EvedOptions $ \case
        [] -> [method]
        _  -> mempty

