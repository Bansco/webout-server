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

module Api.Cast.Models
  ( CreateResponse (..),
    Frame (..),
    ID (..),
    Token (..),
    Channel (..),
    Channels (..),
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as H
import qualified Data.IntMap as IM
import Data.Text (Text)
import GHC.Generics
import General.Util (dropLabelPrefix)
import qualified Network.WebSockets.Connection as Ws
import Text.Casing (camel)
import Prelude

-- -- Models ---
-- TODO: Random instance
newtype ID = ID Text
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, H.Hashable)

-- TODO: Random instance
newtype Token = Token Text
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

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

-- { frame: string, data: string }
-- { frame: string, data: string }
data Frame = Frame
  { frameTime :: Text, -- TODO
    frameData :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Json.ToJSON Frame where
  toJSON = Json.genericToJSON $ dropLabelPrefix "frame"

instance Json.FromJSON Frame where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "frame"

data Channel = Channel
  { chanIsClosed :: Bool,
    chanFrames :: [Frame],
    chanToken :: Token,
    chanCounter :: STM.TVar Int,
    chanConns :: IM.IntMap Ws.Connection
  }

type Channels = HM.HashMap ID Channel
