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

module Api.Session
  ( SessionAPI,
    sessionApi,
    sessionServer,
  )
where

import Api.Session.Models
import Config (AppT (..), Config (..))
import Control.Concurrent.STM (STM)
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import Control.Monad ((<=<))
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
import General.Util (filter, whenJust, (<&>), (<|>))
import qualified General.WebSockets as Ws
import qualified Network.URI as URI
import qualified Network.WebSockets as Ws
import Servant ((:<|>) (..), (:>))
import qualified Servant
import Servant.API.WebSocket (WebSocketPending)
import qualified System.Random as Random
import Prelude hiding (filter)

type SessionAPI = CreateRoute :<|> SessionWsRoute

type CreateRoute =
  "create"
    :> Servant.Get '[Servant.JSON] CreateResponse

type SessionWsRoute =
  "ws"
    :> Servant.Capture "short" ID
    :> Servant.QueryParam "token" Token
    :> WebSocketPending

sessionServer :: MonadIO m => Servant.ServerT SessionAPI (AppT m)
sessionServer = createHandler :<|> sessionWsHandler

sessionApi :: Servant.Proxy SessionAPI
sessionApi = Servant.Proxy

createHandler :: MonadIO m => AppT m CreateResponse
createHandler = do
  channels <- asks configSessionChannels
  sessionId <- liftIO $ ID . UUID.toText <$> Random.randomIO
  token <- liftIO $ Token . UUID.toText <$> Random.randomIO
  url <- sessionUrl sessionId <$> asks configClientUrl
  counter <- liftIO $ STM.newTVarIO 0
  let newChan = Channel False [] token counter IM.empty
  liftIO $ STM.atomically $ STM.modifyTVar channels $ HM.insert sessionId newChan
  pure $ CreateResponse sessionId url token

-- Emitter client
sessionWsHandler :: MonadIO m => ID -> Maybe Token -> Ws.PendingConnection -> AppT m ()
sessionWsHandler sessionId (Just token) pendingC = do
  channels <- asks configSessionChannels
  mbChannel <- liftIO $ HM.lookup sessionId <$> STM.readTVarIO channels
  case mbChannel of
    Nothing -> liftIO $ Ws.rejectNotFound pendingC
    Just chan | chanIsClosed chan -> liftIO $ Ws.rejectNotFound pendingC
    Just chan | chanToken chan /= token -> liftIO $ Ws.rejectUnauthorized pendingC
    Just _ -> do
      c <- liftIO $ Ws.acceptRequest pendingC
      liftIO $
        Exception.finally
          (handleEmitterBroadcasting c sessionId channels)
          (cleanupSessionChannel sessionId channels)
-- Listener client
sessionWsHandler sessionId Nothing pendingC = do
  channels <- asks configSessionChannels
  mbListenerId <- liftIO $ STM.atomically $ getChanId channels sessionId

  case mbListenerId of
    Nothing -> liftIO $ Ws.rejectNotFound pendingC
    Just listenerId -> do
      c <- liftIO $ Ws.acceptRequest pendingC
      liftIO $ STM.atomically $ insertListener channels sessionId listenerId c

      liftIO $
        Exception.finally
          (pingThreadWithBlock c)
          (cleanupListener c sessionId listenerId channels)

pingThreadWithBlock :: Ws.Connection -> IO ()
pingThreadWithBlock c = Ws.pingThread c (Ws.receiveData c :: IO Text)

handleEmitterBroadcasting :: Ws.Connection -> ID -> STM.TVar Channels -> IO ()
handleEmitterBroadcasting c sessionId channels = Ws.pingThread c $
  Monad.forever $ do
    msg <- Ws.receiveData c
    mbChannel <- HM.lookup sessionId <$> STM.readTVarIO channels
    whenJust mbChannel $ broadcast msg

cleanupSessionChannel :: ID -> STM.TVar Channels -> IO ()
cleanupSessionChannel sessionId channels = do
  mbListeners <- STM.atomically (disconnectChan channels sessionId)
  Monad.void $ whenJust mbListeners broadcastClose

cleanupListener :: Ws.Connection -> ID -> Int -> STM.TVar Channels -> IO ()
cleanupListener c sessionId listenerId channels = do
  STM.atomically $ disconnectListener channels sessionId listenerId
  Ws.sendClose c ("Bye" :: Text)

broadcast :: Text -> Channel -> IO ()
broadcast msg chan = Monad.forM_ (chanListeners chan) $ \c ->
  Exception.catch (Ws.sendTextData c msg) Ws.ignoreConnectionException

broadcastClose :: [Ws.Connection] -> IO ()
broadcastClose listeners = Monad.forM_ listeners $ \c ->
  Exception.catch (Ws.sendClose c ("Bye" :: Text)) Ws.ignoreConnectionException

getChanId :: STM.TVar Channels -> ID -> STM (Maybe Int)
getChanId channels sessionId = do
  mbChannel <- filter (not . chanIsClosed) . HM.lookup sessionId <$> STM.readTVar channels
  whenJust mbChannel $ \chan -> do
    i <- STM.readTVar $ chanListenerIdGen chan
    let next = i + 1
    STM.writeTVar (chanListenerIdGen chan) next
    pure next

insertListener :: STM.TVar Channels -> ID -> Int -> Ws.Connection -> STM ()
insertListener channels sessionId listenerId c =
  STM.modifyTVar channels $ HM.adjust insertConnection sessionId
  where
    insertConnection :: Channel -> Channel
    insertConnection chan = chan {chanListeners = IM.insert listenerId c $ chanListeners chan}

disconnectChan :: STM.TVar Channels -> ID -> STM (Maybe [Ws.Connection])
disconnectChan channels sessionId = do
  mbListeners <- fmap (IM.elems . chanListeners) . HM.lookup sessionId <$> STM.readTVar channels
  STM.modifyTVar channels $ HM.adjust closeChan sessionId
  pure mbListeners
  where
    closeChan :: Channel -> Channel
    closeChan chan = chan {chanIsClosed = True, chanListeners = IM.empty}

disconnectListener :: STM.TVar Channels -> ID -> Int -> STM (Maybe Ws.Connection)
disconnectListener channels sessionId listenerId = do
  mc <- (IM.lookup listenerId . chanListeners <=< HM.lookup sessionId) <$> STM.readTVar channels
  STM.modifyTVar channels $ HM.adjust (removeListener listenerId) sessionId
  pure mc
  where
    removeListener :: Int -> Channel -> Channel
    removeListener listenerId chan =
      chan {chanListeners = IM.delete listenerId $ chanListeners chan}
