{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Eved.QueryParam
    where

import           Data.Functor.Contravariant (Contravariant (..))
import           Data.Functor.Invariant     (Invariant (..),
                                             invmapContravariant, invmapFunctor)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import qualified Web.HttpApiData            as HttpApiData

class QueryParam queryParam where
    auto_ :: (HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a) => queryParam a
    list :: queryParam a -> queryParam [a]
    maybe :: queryParam a -> queryParam (Maybe a)
    defaulted :: a -> queryParam a -> queryParam a

auto :: (HttpApiData.FromHttpApiData a, HttpApiData.ToHttpApiData a, QueryParam queryParam)
     => queryParam a
auto = auto_

integer :: QueryParam queryParam => queryParam Integer
integer = auto_

text :: QueryParam queryParam => queryParam Text
text = auto_

instance QueryParam Proxy where
    auto_ = Proxy
    list = const Proxy
    maybe = const Proxy
    defaulted _ = const Proxy

newtype FromQueryParam a = FromQueryParam
    { fromQueryParam :: [Text] -> Either Text a }

instance Functor FromQueryParam where
    fmap f qp = FromQueryParam (fmap f . fromQueryParam qp)
instance Invariant FromQueryParam where
    invmap = invmapFunctor

instance QueryParam FromQueryParam where
    auto_ =
        FromQueryParam
            { fromQueryParam = \case
                  []  -> Left "Value not found"
                  x:_ -> HttpApiData.parseQueryParam x
            }
    list qp =
        FromQueryParam
            { fromQueryParam = traverse (fromQueryParam qp . pure)
            }
    maybe qp =
        FromQueryParam
            { fromQueryParam = \xs ->
                if null xs
                   then pure Nothing
                   else Just <$> fromQueryParam qp xs
            }
    defaulted defaultValue qp =
        FromQueryParam
            { fromQueryParam = \xs ->
                if null xs
                   then pure defaultValue
                   else fromQueryParam qp xs
            }

newtype ToQueryParam a = ToQueryParam
    { toQueryParam   :: a -> [Text] }
instance Contravariant ToQueryParam where
    contramap g qp = ToQueryParam (toQueryParam qp . g)
instance Invariant ToQueryParam where
    invmap = invmapContravariant

instance QueryParam ToQueryParam where
    auto_ = ToQueryParam $ pure . HttpApiData.toQueryParam
    list qp = ToQueryParam $ (=<<) (toQueryParam qp)
    maybe qp = ToQueryParam $ \case
            Just a  -> toQueryParam qp a
            Nothing -> []

    defaulted _ qp = ToQueryParam $ toQueryParam qp
