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
    , EvedScottyT
    , scottyServer
    )
    where

import           Web.Eved.Client
import           Web.Eved.Internal
import           Web.Eved.Server
