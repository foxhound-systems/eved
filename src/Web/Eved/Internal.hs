{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}

module Web.Eved.Internal
    where

import           Data.List.NonEmpty   (NonEmpty)
import           Data.Text            (Text)
import           Network.HTTP.Types   (Status, StdMethod (..), status200)
import           Web.Eved.ContentType (ContentType)
import           Web.Eved.QueryParam  (QueryParam)
import           Web.Eved.UrlElement  (UrlElement)

data (:<|>) a b = a :<|> b
infixl 4 :<|>

class Eved api m | api -> m where
    (.<|>) :: api a -> api b -> api (a :<|> b)

    lit :: Text -> api a -> api a
    capture :: Text -> UrlElement a -> api b -> api (a -> b)
    reqBody :: NonEmpty (ContentType a) -> api b -> api (a -> b)
    queryParam :: Text -> QueryParam a -> api b -> api (a -> b)
    verb :: StdMethod -> Status -> NonEmpty (ContentType a) -> api (m a)

(.</>) :: Eved api m => (api a -> api b) -> api a -> api b
(.</>) = ($)
infixr 5 .</>

get, post, put, patch, delete :: Eved api m => NonEmpty (ContentType a) -> api (m a)
get = verb GET status200
post = verb POST status200
put = verb PUT status200
patch = verb PATCH status200
delete = verb DELETE status200

