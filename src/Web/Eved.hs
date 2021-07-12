{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Eved
    ( I.Eved
    , (I.:<|>)(..)
    , (.</>)
    , (.<|>)
    , lit
    , capture
    , reqBody
    , queryParam
    , verb
    , get
    , post
    , put
    , patch
    , delete
    , runClientIO
    , runClient
    , noContext
    , withContext
    , ClientM
    , EvedServerT
    , server
    )
    where

import           Control.Applicative   (liftA2)
import           Control.Monad.Reader  (Reader, runReader)
import           Data.Function         ((&))
import           Data.Functor.Identity (Identity (..))
import           Data.List.NonEmpty    (NonEmpty)
import           Data.Text             (Text)
import           Network.HTTP.Types    (Status, StdMethod (..), status200)
import           Web.Eved.Client
import qualified Web.Eved.ContentType  as CT
import qualified Web.Eved.Internal     as I
import qualified Web.Eved.QueryParam   as QP
import           Web.Eved.Server
import qualified Web.Eved.UrlElement   as UE


-- |Unwrap an api that requires no context.
-- If none of the combinators that were used required any context use this function
-- to unwrap the api
noContext :: I.Eved api m => Identity (api a) -> api a
noContext = runIdentity

withContext :: I.Eved api m => ctx -> (ctx -> api a) -> api a
withContext = (&)

-- |Combine two sub-api's by trying the left api first and then the right api second.
(.<|>) :: (I.Eved api m, Applicative f) => f (api a) -> f (api b) -> f (api (a I.:<|> b))
(.<|>) =
    liftA2 (I..<|>)

-- |Add a Literal string to the path of the api
lit :: (I.Eved api m, Applicative f) => Text -> f (api a) -> f (api a)
lit l = fmap (I.lit l)

-- |Add a url capture with a given name and UrlElement decoder/encoder
capture :: (I.Eved api m, Applicative f) => Text -> f (UE.UrlElement a) -> f (api b) -> f (api (a -> b))
capture t u next = I.capture t <$> u <*> next

-- |Add a request body parser for the given content types
-- The Content-Type header will be examined to assist in content negotiation.
reqBody :: (I.Eved api m, Applicative f) => NonEmpty (f (CT.ContentType a)) -> f (api b) -> f (api (a -> b))
reqBody ctyps next = I.reqBody <$> sequenceA ctyps <*> next

-- |A single query param that is required to exist. If the argument is not required use QP.maybe
queryParam :: (I.Eved api m, Applicative f) => Text -> f (QP.QueryParam a) -> f (api b) -> f (api (a -> b))
queryParam t q next = I.queryParam t <$> q <*> next

-- |A list of query params with the same name (may return an empty list if the param is not specified ever)
queryParams :: (I.Eved api m, Applicative f) => Text -> f (QP.QueryParam a) -> f (api b) -> f (api ([a] -> b))
queryParams t q next = I.queryParam t <$> QP.list q <*> next

-- |The leaf node of most routes, this will specify the HTTP Verb and Status along with a list of ContentType encoder/decoders.
-- The Allow header in the request will be examined to determine a suitable response Content-Type
verb :: (I.Eved api m, Applicative f) => StdMethod -> Status -> NonEmpty (f (CT.ContentType a)) -> f (api (m a))
verb m s ctyps = fmap (I.verb m s) (sequenceA ctyps)

get, post, put, patch, delete :: (I.Eved api m, Applicative f) => NonEmpty (f (CT.ContentType a)) -> f (api (m a))
-- | HTTP GET -- see verb for more info
get = verb GET status200
-- | HTTP POST -- see verb for more info
post = verb POST status200
-- | HTTP PUT -- see verb for more info
put = verb PUT status200
-- | HTTP PATCH -- see verb for more info
patch = verb PATCH status200
-- | HTTP DELETE -- see verb for more info
delete = verb DELETE status200

-- |A Segment seperator to be used between path segments akin to / in a url
-- e.g. lit "hello" .</> capture "name" UE.text .</> get [CT.json @Text]
(.</>) :: (Applicative f, I.Eved api m) =>  (f (api a) -> f (api b)) -> f (api a) -> f (api b)
(.</>) = ($)
infixr 5 .</>

