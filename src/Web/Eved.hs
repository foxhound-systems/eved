{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Web.Eved
    ( Eved
    , (:<|>)(..)
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
    , ClientM
    , EvedServerT
    , server
    , ContentType
    , UrlElement
    , QueryParam
    )
    where

import           Control.Applicative  (liftA2)
import           Control.Monad.Reader (Reader, runReader)
import           Data.Function        ((&))
import           Data.List.NonEmpty   (NonEmpty)
import           Data.Text            (Text)
import           Network.HTTP.Types   (Status, StdMethod (..), status200)
import           Web.Eved.Client
import qualified Web.Eved.ContentType as CT
import           Web.Eved.Internal
import qualified Web.Eved.QueryParam  as QP
import           Web.Eved.Server
import qualified Web.Eved.UrlElement  as UE

-- |A list of query params with the same name (may return an empty list if the param is not specified ever)
queryParams :: (QP.QueryParam (QueryParam api), Eved api m) => Text -> QueryParam api a -> api b -> api ([a] -> b)
queryParams t q = queryParam t (QP.list q)

get, post, put, patch, delete :: Eved api m => NonEmpty (ContentType api a) -> api (m a)
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
(.</>) :: Eved api m => (api a -> api b) -> api a -> api b
(.</>) = ($)
infixr 5 .</>

