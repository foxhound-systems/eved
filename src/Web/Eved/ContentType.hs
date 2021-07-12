{-# LANGUAGE OverloadedStrings #-}
module Web.Eved.ContentType
    where

import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor       (first)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NE
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Media
import           Network.HTTP.Types

data ContentType a = ContentType
    { toContentType   :: a -> LBS.ByteString
    , fromContentType :: LBS.ByteString -> Either Text a
    , mediaTypes      :: NonEmpty MediaType
    }

json :: (FromJSON a, ToJSON a, Applicative f) => f (ContentType a)
json = pure $ ContentType
    { toContentType = encode
    , fromContentType = first T.pack . eitherDecode
    , mediaTypes = NE.fromList ["application" // "json"]
    }

acceptHeader :: NonEmpty (ContentType a) -> Header
acceptHeader ctypes = (hAccept, renderHeader $ NE.toList $ mediaTypes =<< ctypes)

contentTypeHeader :: ContentType a -> Header
contentTypeHeader ctype = (hContentType, renderHeader $ NE.head $ mediaTypes ctype)

collectMediaTypes :: (ContentType a -> MediaType -> b) -> NonEmpty (ContentType a) -> [b]
collectMediaTypes f ctypes =
    NE.toList $ ctypes >>= (\ctype ->
      fmap (f ctype) (mediaTypes ctype)
    )

chooseAcceptCType :: NonEmpty (ContentType a) -> BS.ByteString -> Maybe (MediaType, a -> LBS.ByteString)
chooseAcceptCType ctypes =
    mapAcceptMedia $ collectMediaTypes (\ctype x -> (x, (x, toContentType ctype))) ctypes

chooseContentCType :: NonEmpty (ContentType a) -> BS.ByteString -> Maybe (LBS.ByteString -> Either Text a)
chooseContentCType ctypes =
    mapContentMedia $ collectMediaTypes (\ctype x -> (x, fromContentType ctype)) ctypes
