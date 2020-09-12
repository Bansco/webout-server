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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM)
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
import General.Util ((<&>), (<|>))
import qualified Network.URI as URI
import qualified Network.WebSockets as Ws
import Servant ((:<|>) (..), (:>))
import qualified Servant
import Servant.API.WebSocket (WebSocketPending)
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
    :> WebSocketPending

castServer :: MonadIO m => Servant.ServerT CastAPI (AppT m)
castServer = createHandler :<|> castWsHandler

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
  pure $ CreateResponse (ID uuid) (clientUrl <> "/c/" <> uuid) (Token token)

channelNotFound :: Ws.PendingConnection -> IO ()
channelNotFound pendingC =
  Ws.rejectRequestWith pendingC $
    Ws.defaultRejectRequest {Ws.rejectCode = 404, Ws.rejectMessage = "Not found"}

unauthorized :: Ws.PendingConnection -> IO ()
unauthorized pendingC =
  Ws.rejectRequestWith pendingC $
    Ws.defaultRejectRequest {Ws.rejectCode = 401, Ws.rejectMessage = "Unauthorized"}

castWsHandler :: MonadIO m => Text -> Maybe Text -> Ws.PendingConnection -> AppT m ()
castWsHandler id (Just token) pendingC = do
  channels <- asks configCastChannels
  mbChannel <- liftIO $ HM.lookup (ID id) <$> STM.readTVarIO channels
  case mbChannel of
    Nothing -> liftIO $ channelNotFound pendingC
    Just chan -> do
      -- TODO catch close error and cleanup
      Monad.when (chanToken chan /= Token token) $ liftIO $ unauthorized pendingC
      c <- liftIO $ Ws.acceptRequest pendingC
      liftIO $
        flip
          Exception.finally
          ( do
              clients <- fromMaybe [] <$> STM.atomically (disconnectChan channels (ID id))
              -- TODO errors on close clients
              Monad.forM_ clients $ flip Ws.sendClose ("Bye" :: Text)
          )
          $ Ws.withPingThread c 30 mempty $
            Monad.forever $ do
              msg :: Text <- Ws.receiveData c
              -- TODO how to avoid reading channel if we know channel is defined here
              mbChannel <- HM.lookup (ID id) <$> STM.readTVarIO channels
              case mbChannel of
                Nothing -> putStrLn "channel lost" -- TODO <----
                Just chan -> broadcast chan msg
castWsHandler id Nothing pendingC = do
  channels <- asks configCastChannels
  mbClientId <- liftIO $ STM.atomically $ getChanId channels (ID id)

  case mbClientId of
    Nothing -> liftIO $ channelNotFound pendingC
    Just clientId -> do
      c <- liftIO $ Ws.acceptRequest pendingC
      liftIO $ STM.atomically $ inserClient channels (ID id) clientId c

      -- TODO how to keep this connected and pinging without receiving ????
      liftIO $
        Exception.finally (pingThread c) $ do
          STM.atomically $ disconnectClient channels (ID id) clientId
          Ws.sendClose c ("Bye" :: Text) -- TODO what to send as close?

pingThread :: Ws.Connection -> IO ()
pingThread c = Ws.withPingThread c 30 mempty $ Monad.void (Ws.receiveData c :: IO Text)

broadcast :: Channel -> Text -> IO ()
broadcast chan msg = Monad.forM_ (chanConns chan) $ \c ->
  Exception.catch (Ws.sendTextData c msg) catchAll
  where
    -- we ignore the disconnection, as the listener endpoint should handle it
    catchAll :: Ws.ConnectionException -> IO ()
    catchAll _ = mempty

getChanId :: STM.TVar Channels -> ID -> STM (Maybe Int)
getChanId channels chanId = do
  mbChannel <- HM.lookup chanId <$> STM.readTVar channels
  case mbChannel of
    Just chan -> do
      i <- STM.readTVar $ chanCounter chan
      let next = i + 1
      STM.writeTVar (chanCounter chan) next
      pure $ Just next
    Nothing -> pure Nothing

inserClient :: STM.TVar Channels -> ID -> Int -> Ws.Connection -> STM ()
inserClient channels chanId clientId c =
  STM.modifyTVar channels $ HM.adjust insertConnection chanId
  where
    insertConnection :: Channel -> Channel
    insertConnection chan = chan {chanConns = IM.insert clientId c $ chanConns chan}

disconnectChan :: STM.TVar Channels -> ID -> STM (Maybe [Ws.Connection])
disconnectChan channels chanId = do
  STM.modifyTVar channels $ HM.adjust closeChan chanId
  fmap (IM.elems . chanConns) . HM.lookup chanId <$> STM.readTVar channels
  where
    closeChan :: Channel -> Channel
    closeChan chan = chan {chanIsClosed = True}

disconnectClient :: STM.TVar Channels -> ID -> Int -> STM (Maybe Ws.Connection)
disconnectClient channels chanId clientId = do
  mc <- ((IM.lookup clientId . chanConns) <=< HM.lookup chanId) <$> STM.readTVar channels
  STM.modifyTVar channels $ HM.adjust (removeClient clientId) chanId
  pure mc
  where
    removeClient :: Int -> Channel -> Channel
    removeClient clientId chan =
      chan {chanConns = IM.delete clientId $ chanConns chan}

redirectTo :: MonadIO m => Text -> AppT m ()
redirectTo url =
  Servant.throwError $
    Servant.err302 {Servant.errHeaders = [("Location", encodeUtf8 url)]}
