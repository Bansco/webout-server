{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( app,
  )
where

import Api.Cast (CastAPI, castApi, castServer)
import Config (AppT (..), Config (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Data.Text (Text)
import Servant
  ( (:<|>) (..),
    (:>),
  )
import qualified Servant
import Servant.Server
import Prelude

weboutApp :: Config -> Application
weboutApp cfg = Servant.serve appApi (appToServer cfg)

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: Config -> Servant.Server AppAPI
appToServer cfg = hoistServer appApi (convertApp cfg) appServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

type AppAPI = HealthzRoute :<|> "api" :> "cast" :> CastAPI

healthzHandler :: MonadIO m => AppT m Text
healthzHandler = pure "200 Ok"

type HealthzRoute =
  "healthz"
    :> Servant.Get '[Servant.PlainText] Text

appServer :: MonadIO m => Servant.ServerT AppAPI (AppT m)
appServer = healthzHandler :<|> castServer

appApi :: Servant.Proxy AppAPI
appApi = Servant.Proxy

app :: Config -> Application
app cfg = serve appApi $ appToServer cfg
