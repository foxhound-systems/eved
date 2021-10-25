module Web.Eved.UrlElement
    where

import           Data.Proxy
import           Data.Text       (Text)
import qualified Web.HttpApiData as HttpApiData

class UrlElement urlElement where
    auto :: (HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a) => urlElement a

integer :: UrlElement urlElement => urlElement Integer
integer = auto

text :: UrlElement urlElement => urlElement Text
text = auto

newtype FromUrlElement a = FromUrlElement
    { fromUrlPiece :: Text -> Either Text a }
newtype ToUrlElement a = ToUrlElement
    { toUrlPiece   :: a -> Text }

instance UrlElement Proxy where
    auto = Proxy
instance UrlElement FromUrlElement where
    auto = FromUrlElement HttpApiData.parseUrlPiece
instance UrlElement ToUrlElement where
    auto = ToUrlElement HttpApiData.toUrlPiece

