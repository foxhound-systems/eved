{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Eved.QueryParam
    where

import           Data.Text       (Text)
import qualified Web.HttpApiData as HttpApiData

data QueryParam a = QueryParam
    { fromQueryParam :: [Text] -> Either Text a
    , toQueryParam   :: a -> [Text]
    }

auto :: (Applicative f, HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a) => f (QueryParam a)
auto = pure $ QueryParam
    { fromQueryParam = \case
          []  -> Left "Value not found"
          x:_ -> HttpApiData.parseQueryParam x
    , toQueryParam = pure . HttpApiData.toQueryParam
    }

list ::  Functor f => f (QueryParam a) -> f (QueryParam [a])
list = fmap $ \qp ->
    QueryParam
        { fromQueryParam = traverse (fromQueryParam qp . pure)
        , toQueryParam = (=<<) (toQueryParam qp)
        }

defaulted :: Functor f => a -> f (QueryParam a) -> f (QueryParam a)
defaulted defaultValue = fmap $ \qp ->
    QueryParam
        { fromQueryParam = \xs ->
            if null xs
               then pure defaultValue
               else fromQueryParam qp xs
        , toQueryParam = toQueryParam qp
        }

maybe :: Functor f => f (QueryParam a) -> f (QueryParam (Maybe a))
maybe = fmap $ \qp ->
    QueryParam
        { fromQueryParam = \xs ->
            if null xs
               then pure Nothing
               else Just <$> fromQueryParam qp xs
        , toQueryParam = \case
            Just a  -> toQueryParam qp a
            Nothing -> []
        }

integer :: Applicative f => f (QueryParam Integer)
integer = auto

text :: Applicative f => f (QueryParam Text)
text = auto

