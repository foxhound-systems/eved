{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

module Web.Eved.Server
    where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor           ((<&>))
import           Data.Maybe             (catMaybes)
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as TL
import           Network.HTTP.Media     (renderHeader)
import           Network.HTTP.Types     (badRequest400, notAcceptable406,
                                         unsupportedMediaType415)
import qualified Web.Eved.ContentType   as CT
import           Web.Eved.Internal
import qualified Web.Eved.QueryParam    as QP
import qualified Web.Eved.UrlElement    as UE
import qualified Web.Scotty             as Scotty

newtype EvedScottyT m a = EvedScottyT
    { unEvedScottyT :: (forall a. m a -> IO a) -> Text -> Scotty.ActionM a -> Scotty.ScottyM ()
    }

scottyServer :: (forall a. m a -> IO a) -> EvedScottyT m a -> a -> Scotty.ScottyM ()
scottyServer nt scottyEved handlers =
    unEvedScottyT scottyEved (liftIO . nt) mempty (pure handlers)

instance Eved (EvedScottyT m) m where
    a .<|> b = EvedScottyT $ \nt r action ->
        let left = unEvedScottyT a nt r (do (f :<|> _) <- action; pure f)
            right = unEvedScottyT b nt r (do (_ :<|> g) <- action; pure g)
        in left <> right

    lit s next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt (r <> "/" <> s) action

    capture s el next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt (r <> "/:" <> s) $ do
            mArg <- fmap (UE.fromUrlPiece el) $ Scotty.param $ TL.fromStrict s
            case mArg of
              Right arg -> fmap ($ arg) action
              Left _    -> Scotty.next

    reqBody ctypes next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt r $ do
            contentTypeH <- Scotty.header "Content-Type"
            let contentTypeBS = maybe "" (encodeUtf8 . TL.toStrict) contentTypeH
            case CT.chooseContentCType ctypes contentTypeBS of
              Just bodyParser  -> do
                  requestBody <- Scotty.body
                  case bodyParser requestBody of
                      Just a ->
                          fmap ($ a) action
                      Nothing ->
                          Scotty.raiseStatus badRequest400 ""
              Nothing ->
                  Scotty.raiseStatus unsupportedMediaType415 ""

    queryParam s el next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt r $ do
            mArg <- fmap (QP.fromQueryParam el) $ Scotty.param $ TL.fromStrict s
            case mArg of
              Right arg -> fmap ($ arg) action
              Left _    -> Scotty.next

    queryParams s qp next = EvedScottyT $ \nt r action ->
        unEvedScottyT next nt r $
            Scotty.params
                <&> filter (\(k, _) -> k == TL.fromStrict s)
                <&> fmap (\(_, v) -> QP.fromQueryParam qp (TL.toStrict v))
                <&> fmap (either (const Nothing) Just)
                <&> catMaybes
                >>= (\x -> fmap ($ x) action)

    verb method status ctypes = EvedScottyT $ \nt r action ->
        Scotty.addroute method (fromString $ T.unpack r) $ do
            liftIO . print =<< Scotty.request
            acceptH <- Scotty.header "Accept"
            let acceptBS = maybe "" (encodeUtf8 . TL.toStrict) acceptH
            case CT.chooseAcceptCType ctypes acceptBS of
              Just (ctype, renderContent) -> do
                  Scotty.setHeader "Content-Type" (TL.fromStrict $ decodeUtf8 $ renderHeader ctype)
                  Scotty.status status
                  join (fmap (liftIO . nt) action) >>= (Scotty.raw . renderContent)
              Nothing ->
                  Scotty.raiseStatus notAcceptable406 mempty
