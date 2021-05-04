{-# LANGUAGE OverloadedStrings #-}
module Web.Eved.QueryParam
    where

import           Data.Text       (Text)
import qualified Web.HttpApiData as HttpApiData

data QueryParam a = QueryParam
    { fromQueryParam :: Text -> Either Text a
    , toQueryParam   :: a -> Maybe Text
    }

auto :: (HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a) => QueryParam a
auto = QueryParam
    { fromQueryParam = HttpApiData.parseQueryParam
    , toQueryParam = Just . HttpApiData.toQueryParam
    }

maybe :: QueryParam a -> QueryParam (Maybe a)
maybe qp =
    QueryParam
        { fromQueryParam = either (const $ Right Nothing) (Right . Just) . (fromQueryParam qp)
        , toQueryParam = (=<<) (toQueryParam qp)
        }

integer :: QueryParam Integer
integer = auto

text :: QueryParam Text
text = auto
