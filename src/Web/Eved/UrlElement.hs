module Web.Eved.UrlElement
    where

import           Data.Text       (Text)
import qualified Web.HttpApiData as HttpApiData

data UrlElement a = UrlElement
    { fromUrlPiece :: Text -> Either Text a
    , toUrlPiece   :: a -> Text
    }

auto :: (HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a) => UrlElement a
auto = UrlElement
    { fromUrlPiece = HttpApiData.parseUrlPiece
    , toUrlPiece = HttpApiData.toUrlPiece
    }

integer :: UrlElement Integer
integer = auto

text :: UrlElement Text
text = auto
