{-# LANGUAGE OverloadedStrings #-}
module Web.Eved.ContentType
    where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NE
import           Network.HTTP.Media
import           Network.HTTP.Types

data ContentType a = ContentType
    { toContentType   :: a -> LBS.ByteString
    , fromContentType :: LBS.ByteString -> Maybe a
    , mediaTypes      :: NonEmpty MediaType
    }

json :: (FromJSON a, ToJSON a) => ContentType a
json = ContentType
    { toContentType = encode
    , fromContentType = decode
    , mediaTypes = pure ("application" // "json")
    }

acceptHeader :: NonEmpty (ContentType a) -> Header
acceptHeader ctypes = (hAccept, renderHeader $ NE.toList $ join $ fmap mediaTypes ctypes)

contentTypeHeader :: ContentType a -> Header
contentTypeHeader ctype = (hContentType, renderHeader $ NE.head $ mediaTypes ctype)

collectMediaTypes :: (ContentType a -> MediaType -> b) -> NonEmpty (ContentType a) -> [b]
collectMediaTypes f ctypes =
      NE.toList $ join $ fmap (\ctype ->
          fmap (f ctype) (mediaTypes ctype)
        ) ctypes

chooseAcceptCType :: NonEmpty (ContentType a) -> BS.ByteString -> Maybe (MediaType, a -> LBS.ByteString)
chooseAcceptCType ctypes =
    mapAcceptMedia $ collectMediaTypes (\ctype x -> (x, (x, toContentType ctype))) ctypes

chooseContentCType :: NonEmpty (ContentType a) -> BS.ByteString -> Maybe (LBS.ByteString -> Maybe a)
chooseContentCType ctypes =
    mapContentMedia $ collectMediaTypes (\ctype x -> (x, fromContentType ctype)) ctypes
