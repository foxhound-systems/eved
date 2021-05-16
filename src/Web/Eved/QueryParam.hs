{-# LANGUAGE OverloadedStrings #-}
module Web.Eved.QueryParam
    where

import           Data.Text       (Text)
import qualified Web.HttpApiData as HttpApiData

data QueryParam a = QueryParam
    { fromQueryParam :: Text -> Either Text a
    , toQueryParam   :: a -> Maybe Text
    }

auto :: (Applicative f, HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a) => f (QueryParam a)
auto = pure $ QueryParam
    { fromQueryParam = HttpApiData.parseQueryParam
    , toQueryParam = Just . HttpApiData.toQueryParam
    }

maybe :: Functor f => f (QueryParam a) -> f (QueryParam (Maybe a))
maybe = fmap $ \qp ->
    QueryParam
    { fromQueryParam = either (const $ Right Nothing) (Right . Just) . (fromQueryParam qp)
    , toQueryParam = (=<<) (toQueryParam qp)
    }

integer :: Applicative f => f (QueryParam Integer)
integer = auto

text :: Applicative f => f (QueryParam Text)
text = auto

