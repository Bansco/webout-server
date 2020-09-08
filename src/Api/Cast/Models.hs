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
  )
where

import qualified Data.Aeson as Json
import Data.HashMap.Strict as HM
import Data.Text (Text)
import GHC.Generics
import General.Util (dropLabelPrefix)
import Network.WebSockets.Connection as Ws
import Text.Casing (camel)
import Prelude

-- -- Models ---
newtype ID = ID Text
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

newtype Token = Token Text
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

data CreateResponse = CreateResponse
  { createId :: ID,
    createToken :: Token
  }
  deriving stock (Generic, Show)

instance Json.ToJSON CreateResponse where
  toJSON = Json.genericToJSON $ dropLabelPrefix "create"

instance Json.FromJSON CreateResponse where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix "create"

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
    chanConns :: HM.HashMap ID Ws.Connection
  }

type Channels = HM.HashMap ID Channel
