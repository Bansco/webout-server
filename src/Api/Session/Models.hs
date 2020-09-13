{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Session.Models
  ( CreateResponse (..),
    Frame (..),
    ID (..),
    Token (..),
    Channel (..),
    Channels (..),
    unId,
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Json
import Data.Either (Either (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import qualified Data.IntMap as IM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import General.Util (dropLabelPrefix)
import qualified Network.WebSockets.Connection as Ws
import qualified Servant
import Text.Casing (camel)
import Prelude

newtype ID = ID Text
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, H.Hashable)

unId :: ID -> Text
unId (ID id) = id

instance Servant.FromHttpApiData ID where
  parseUrlPiece = Right . ID

newtype Token = Token Text
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

instance Servant.FromHttpApiData Token where
  parseUrlPiece = Right . Token

data CreateResponse = CreateResponse
  { createId :: ID,
    createUrl :: Text,
    createToken :: Token
  }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateResponse where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateResponse where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

-- TODO
data Frame = Frame
  { frameTime :: Text,
    frameData :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Json.ToJSON Frame where
  toJSON = Json.genericToJSON $ dropLabelPrefix "frame"

instance Json.FromJSON Frame where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "frame"

-- | A Channel contains the session information: listeners, token, data sent.
data Channel = Channel
  { chanIsClosed :: Bool,
    chanFrames :: [Frame],
    chanToken :: Token,
    chanListenerIdGen :: STM.TVar Int,
    chanListeners :: IM.IntMap Ws.Connection
  }

type Channels = HM.HashMap ID Channel
