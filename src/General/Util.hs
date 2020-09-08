module General.Util
  ( (<&>),
    (<|>),
    dropLabelPrefix,
    camelTags,
    stripPrefix,
  )
where

import Control.Applicative (liftA2)
import qualified Data.Aeson as Json
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Text.Casing (camel)
import Prelude

(<&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&>) = liftA2 (&&)

(<|>) :: Applicative f => f Bool -> f Bool -> f Bool
(<|>) = liftA2 (||)

dropLabelPrefix :: String -> Json.Options
dropLabelPrefix prefix =
  Json.defaultOptions {Json.fieldLabelModifier = camel . stripPrefix prefix}

camelTags :: Json.Options
camelTags =
  Json.defaultOptions {Json.constructorTagModifier = camel}

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix str = Maybe.fromMaybe str $ List.stripPrefix prefix str
