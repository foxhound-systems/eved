module Web.Eved.QueryParam
    where

import           Data.Text       (Text)
import qualified Web.HttpApiData as HttpApiData

data QueryParam a = QueryParam
    { fromQueryParam :: Text -> Either Text a
    , toQueryParam   :: a -> Text
    }

auto :: (HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a) => QueryParam a
auto = QueryParam
    { fromQueryParam = HttpApiData.parseQueryParam
    , toQueryParam = HttpApiData.toQueryParam
    }

integer :: QueryParam Integer
integer = auto

text :: QueryParam Text
text = auto
