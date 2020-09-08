module Main where

import Data.Map
import Data.Text (Text)
import Network.WebSockets.Connection as Ws

main :: IO ()
main = putStrLn "Hola"

newtype ID = ID Text deriving (Show, Eq)

newtype Token = Token Text deriving (Show, Eq)

type Channels = Map ID Channel

data Frame = Frame
  { frameTime :: Text, -- TODO
    frameData :: Text
  }

data Channel = Channel
  { chanIsClosed :: Bool,
    chanFrames :: [Frame],
    chanToken :: Token,
    chanConns :: Map ID Ws.Connection
  }
