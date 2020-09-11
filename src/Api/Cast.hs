{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api.Cast
  ( CastAPI,
    castApi,
    castServer,
  )
where

import Api.Cast.Models
import Config (AppT (..), Config (..))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import qualified Data.Char as Char
import Data.Conduit (ConduitT, yield)
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.UUID as UUID
import General.Util ((<&>), (<|>))
import qualified Network.URI as URI
import qualified Network.WebSockets as Ws
import Servant ((:<|>) (..), (:>))
import qualified Servant
import Servant.API.WebSocket (WebSocket)
import qualified System.Random as Random
import Prelude

type CastAPI = CreateRoute :<|> CastWsRoute

type CreateRoute =
  "create"
    :> Servant.Get '[Servant.JSON] CreateResponse

type CastWsRoute =
  "ws"
    :> Servant.Capture "short" Text -- TODO use Api.Cast.Models.ID
    :> Servant.QueryParam "token" Text
    :> WebSocket

castServer :: MonadIO m => Servant.ServerT CastAPI (AppT m)
castServer =
  createHandler
    :<|> castWsHandler

castApi :: Servant.Proxy CastAPI
castApi = Servant.Proxy

createHandler :: MonadIO m => AppT m CreateResponse
createHandler = do
  channels <- asks configCastChannels
  clientUrl <- asks configClientUrl
  uuid <- liftIO $ UUID.toText <$> Random.randomIO
  token <- liftIO $ UUID.toText <$> Random.randomIO
  counter <- liftIO $ STM.newTVarIO 0
  let newChan = Channel False [] (Token token) counter IM.empty
  liftIO $ STM.atomically $ STM.modifyTVar channels $ HM.insert (ID uuid) newChan
  -- pure $ CreateResponse (clientUrl <> "/c/" <> uuid) $ Token token
  pure $ CreateResponse uuid $ Token token

castWsHandler :: MonadIO m => Text -> Maybe Text -> Ws.Connection -> AppT m ()
castWsHandler id (Just token) c = do
  channels <- asks configCastChannels
  mbChannel <- liftIO $ HM.lookup (ID id) <$> STM.readTVarIO channels
  liftIO $ print =<< HM.keys <$> STM.readTVarIO channels
  case mbChannel of
    Nothing -> Servant.throwError Servant.err404
    Just chan -> do
      -- TODO catch close error and cleanup
      Monad.when (chanToken chan /= Token token) $ Servant.throwError Servant.err401
      liftIO $
        flip Exception.finally (STM.atomically $ disconnectChan channels (ID id)) $
          Monad.forever $ do
            msg :: Text <- Ws.receiveData c
            liftIO $ putStrLn $ Text.unpack $ "Go Message:\n" <> msg
            mbChannel <- HM.lookup (ID id) <$> STM.readTVarIO channels
            -- TODO make this prettier
            -- TODO stop the loop
            case mbChannel of
              Nothing -> pure ()
              Just chan -> broadcast chan msg
castWsHandler id Nothing c = do
  -- TODO catch close error and cleanup
  channels <- asks configCastChannels
  mbClientId <- liftIO $ STM.atomically $ connect channels (ID id) c

  liftIO $ print =<< HM.keys <$> STM.readTVarIO channels
  liftIO $ print =<< HM.member (ID id) <$> STM.readTVarIO channels
  liftIO $ print mbClientId

  case mbClientId of
    Nothing -> do
      liftIO $ print $ "404 on " <> id
      Servant.throwError Servant.err404
    Just clientId -> do
      liftIO $ print $ "200 on " <> id
      liftIO $
        flip Exception.finally (STM.atomically $ disconnectClient channels (ID id) clientId) $
          -- TODO how to keep this connected and pinging ????
          Ws.withPingThread c 30 Monad.mzero Monad.mzero

broadcast :: Channel -> Text -> IO ()
broadcast chan = Monad.forM_ (chanConns chan) . flip Ws.sendTextData

connect :: STM.TVar Channels -> ID -> Ws.Connection -> STM (Maybe Int)
connect channels chanId c = do
  chans <- STM.readTVar channels
  case HM.lookup chanId chans of
    Nothing -> pure Nothing
    Just chan -> do
      i <- STM.readTVar $ chanCounter chan
      let next = i + 1
      let updatedChan = chan {chanConns = IM.insert next c $ chanConns chan}
      STM.writeTVar channels $ HM.insert chanId updatedChan chans
      STM.writeTVar (chanCounter chan) next
      pure $ Just next
  pure Nothing

disconnectChan :: STM.TVar Channels -> ID -> STM ()
disconnectChan channels = STM.modifyTVar channels . HM.adjust closeChan

disconnectClient :: STM.TVar Channels -> ID -> Int -> STM ()
disconnectClient channels chanId clientId =
  STM.modifyTVar channels $ HM.adjust (removeClient clientId) chanId

-- broadcast :: Channel -> Text ->

closeChan :: Channel -> Channel
closeChan chan = chan {chanIsClosed = True}

removeClient :: Int -> Channel -> Channel
removeClient clientId chan =
  chan {chanConns = IM.delete clientId $ chanConns chan}

redirectTo :: MonadIO m => Text -> AppT m ()
redirectTo url =
  Servant.throwError $
    Servant.err302 {Servant.errHeaders = [("Location", encodeUtf8 url)]}
