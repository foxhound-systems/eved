{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Eved.Header
    where

import           Data.ByteString    (ByteString)
import           Data.Text          (Text)
import qualified Network.HTTP.Types as HTTP
import           Web.HttpApiData    (FromHttpApiData (parseHeader),
                                     ToHttpApiData (toHeader))

data Header a = Header
    { toHeaderValue   :: a -> Maybe ByteString
    , fromHeaderValue :: Maybe ByteString -> Either Text a
    }

auto :: (Applicative f, ToHttpApiData a, FromHttpApiData a) => f (Header a)
auto = pure $ Header
    { toHeaderValue = Just . toHeader
    , fromHeaderValue = Prelude.maybe (Left "No Header Found") parseHeader
    }

maybe :: Functor f => f (Header a) -> f (Header (Maybe a))
maybe = fmap $ \h ->
    Header
        { fromHeaderValue = \case
            Just v  -> Just <$> fromHeaderValue h (Just v)
            Nothing -> pure Nothing
        , toHeaderValue = \case
            Just a  -> toHeaderValue h a
            Nothing -> Nothing
        }
