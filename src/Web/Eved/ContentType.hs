{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Web.Eved.ContentType
    where

import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor       (first)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NE
import           Data.Proxy
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Media
import           Network.HTTP.Types

class ContentTypeJSON ctype where
   json_ :: (FromJSON a, ToJSON a) => ctype a

-- Fix type application
json :: (FromJSON a, ToJSON a, ContentTypeJSON ctype) => ctype a
json = json_

data ContentType a = ContentType
    { toContentType   :: a -> (RequestHeaders, LBS.ByteString)
    , fromContentType :: (RequestHeaders, LBS.ByteString) -> Either Text a
    , mediaTypes      :: NonEmpty MediaType
    }

instance ContentTypeJSON Proxy where
    json_ = Proxy
instance ContentTypeJSON ContentType where
    json_ = ContentType
        { toContentType = (mempty,) . encode
        , fromContentType = first T.pack . eitherDecode . snd
        , mediaTypes = NE.fromList ["application" // "json"]
        }


class ContentTypeWithHeaders ctype where
    withHeaders :: ctype a -> ctype (WithHeaders a)
instance ContentTypeWithHeaders Proxy where
    withHeaders = const Proxy
instance ContentTypeWithHeaders ContentType where
    withHeaders ctype =
        ContentType
            { toContentType = \(WithHeaders rHeaders val) -> (rHeaders, snd $ toContentType ctype val)
            , fromContentType = \(rHeaders, rBody) -> fmap (WithHeaders rHeaders) (fromContentType ctype (mempty, rBody))
            , mediaTypes = mediaTypes ctype
            }

data WithHeaders a = WithHeaders RequestHeaders a

addHeaders :: RequestHeaders -> a -> WithHeaders a
addHeaders = WithHeaders

acceptHeader :: NonEmpty (ContentType a) -> Header
acceptHeader ctypes = (hAccept, renderHeader $ NE.toList $ mediaTypes =<< ctypes)

contentTypeHeader :: ContentType a -> Header
contentTypeHeader ctype = (hContentType, renderHeader $ NE.head $ mediaTypes ctype)

collectMediaTypes :: (ContentType a -> MediaType -> b) -> NonEmpty (ContentType a) -> [b]
collectMediaTypes f ctypes =
    NE.toList $ ctypes >>= (\ctype ->
      fmap (f ctype) (mediaTypes ctype)
    )

chooseAcceptCType :: NonEmpty (ContentType a) -> BS.ByteString -> Maybe (MediaType, a -> (RequestHeaders, LBS.ByteString))
chooseAcceptCType ctypes =
    mapAcceptMedia $ collectMediaTypes (\ctype x -> (x, (x, toContentType ctype))) ctypes

chooseContentCType :: NonEmpty (ContentType a) -> RequestHeaders -> BS.ByteString -> Maybe (LBS.ByteString -> Either Text a)
chooseContentCType ctypes rHeaders =
    mapContentMedia $ collectMediaTypes (\ctype x -> (x, fromContentType ctype . (rHeaders,))) ctypes
