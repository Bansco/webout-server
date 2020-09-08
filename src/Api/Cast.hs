{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Cast
  ( CastAPI,
    castApi,
    castServer,
  )
where

import Api.Cast.Models
import Config (AppT (..), Config (..))
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Class (lift)
import qualified Data.Char as Char
import Data.Conduit (ConduitT, yield)
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import General.Util ((<&>), (<|>))
import qualified Network.URI as URI
import Network.WebSockets (Connection)
import Servant ((:<|>) (..), (:>))
import qualified Servant
import Servant.API.WebSocket (WebSocket)
import Prelude

type CastAPI = CreateRoute :<|> CastWsRoute

type CreateRoute =
  "cast" :> "create"
    :> Servant.Post '[Servant.JSON] CreateResponse

type CastWsRoute = "cast" :> "ws" :> WebSocket

castServer :: MonadIO m => Servant.ServerT CastAPI (AppT m)
castServer =
  createHandler
    :<|> castWsHandler

castApi :: Servant.Proxy CastAPI
castApi = Servant.Proxy

createHandler :: MonadIO m => AppT m CreateResponse
createHandler = undefined

-- TODO: how to use the AppT m ???
castWsHandler :: MonadIO m => Connection -> AppT m ()
castWsHandler = undefined

redirectTo :: MonadIO m => Text -> AppT m ()
redirectTo url =
  Servant.throwError $
    Servant.err302 {Servant.errHeaders = [("Location", encodeUtf8 url)]}
