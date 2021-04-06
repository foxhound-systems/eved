# Eved - A Value Level Servant Replacement

Eved is an API definition eDSL in the spirit of [Servant](https://hackage.haskell.org/package/servant). 
The main difference from Servant is that Eved is a value level API, whereas Servant relies very heavily on fancy type level programming. 

Eved is highly extensible (both in terms of implementations and new terms) thanks to its utilization of the so called `final tagless` encoding.

## Example

### Api Definition

```haskell
import           Data.Text              (Text)
import qualified Web.Eved.ContentType   as CT
import qualified Web.Eved.QueryParam    as QP
import qualified Web.Eved.UrlElement    as UE

import           Web.Eved.Internal

type Api m =
       (Integer -> Integer -> m Integer)
  :<|> (Text    -> Integer -> m Integer)
  :<|> (Text    -> Integer -> m Integer)

api :: Eved api m => api (Api m)
api =
      (lit "v1" .</> capture "captureNum" UE.integer .</> queryParam "arg1" QP.integer .</> get [CT.json @Integer])
 .<|> (lit "v1" .</> capture "captureText" UE.text .</> reqBody [CT.json @Integer] .</> post [CT.json @Integer])
 .<|> (lit "v2" .</> capture "captureText" UE.text .</> queryParam "arg1" QP.integer .</> get [CT.json @Integer])
```

### Client

```haskell
import           Web.Eved.Client

v1CaptureNum :<|> v1CaptureText :<|> v2CaptureText = getClient api "http://localhost:3000"
```

### Server

```haskell
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Web.Eved.Server
import           Web.Scotty

serve :: IO ()
serve = scotty 3000 $ scottyServer api (`runReaderT` (Env 100)) (pure handlers)

data Env = Env
    { envSecret :: Integer
    }

handlers :: (MonadIO m, MonadReader Env m) => Api m
handlers =   (\a b -> pure $ a + b)
        :<|> (\_ i -> fmap ((+) i) (asks envSecret))
        :<|> (\_ i -> pure i)

```

## Prior Art

TODO

## How to extend the language

TODO
