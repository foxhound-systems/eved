module Web.Eved.UrlElement
    where

import           Data.Text       (Text)
import qualified Web.HttpApiData as HttpApiData

data UrlElement a = UrlElement
    { fromUrlPiece :: Text -> Either Text a
    , toUrlPiece   :: a -> Text
    }

auto :: (HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a, Applicative f) => f (UrlElement a)
auto = pure $ UrlElement
    { fromUrlPiece = HttpApiData.parseUrlPiece
    , toUrlPiece = HttpApiData.toUrlPiece
    }

integer :: Applicative f => f (UrlElement Integer)
integer = auto

text :: Applicative f => f (UrlElement Text)
text = auto
