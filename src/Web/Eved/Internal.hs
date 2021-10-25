{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Web.Eved.Internal
    where

import           Data.List.NonEmpty  (NonEmpty)
import           Data.Text           (Text)
import           Network.HTTP.Types  (Status, StdMethod (..), status200)
import           Web.Eved.Header     (Header)
import qualified Web.Eved.QueryParam as QP
import qualified Web.Eved.UrlElement as UE

data (:<|>) a b = a :<|> b
infixl 4 :<|>

class ( QP.QueryParam (QueryParam api)
      , UE.UrlElement (UrlElement api)
      ) => Eved api m | api -> m where
    type UrlElement api :: * -> *
    type ContentType api :: * -> *
    type QueryParam api :: * -> *

    -- |Combine two sub-api's by trying the left api first and then the right api second.
    (.<|>) :: api a -> api b -> api (a :<|> b)

    -- |Add a Literal string to the path of the api
    lit :: Text -> api a -> api a

    -- |Add a url capture with a given name and UrlElement decoder/encoder
    capture :: Text -> UrlElement api a -> api b -> api (a -> b)

    -- |Add a request body parser for the given content types
    -- The Content-Type header will be examined to assist in content negotiation.
    reqBody :: NonEmpty (ContentType api a) -> api b -> api (a -> b)

    -- |A single query param that is required to exist. If the argument is not required use QP.maybe
    queryParam :: Text -> QueryParam api a -> api b -> api (a -> b)

    header :: Text -> Header a -> api b -> api (a -> b)

    -- |The leaf node of most routes, this will specify the HTTP Verb and Status along with a list of ContentType encoder/decoders.
    -- The Allow header in the request will be examined to determine a suitable response Content-Type
    verb :: StdMethod -> Status -> NonEmpty (ContentType api a) -> api (m a)
