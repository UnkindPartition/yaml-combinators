{-# LANGUAGE CPP #-}

module Data.Yaml.Combinators.AesonCompat
  ( Value (..),
    Object,
    Array,
    objDifference,
    objLookup,
    objNull,
    objSingleton,
  )
where

#if MIN_VERSION_aeson(2,0,0)

import Data.Aeson (Array, Object, Value (..))
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)

objDifference :: Object -> KeyMap () -> Object
objDifference = KeyMap.difference

objLookup :: Text -> Object -> Maybe Value
objLookup key object = KeyMap.lookup (Key.fromText key) object

objNull :: Object -> Bool
objNull = KeyMap.null

objSingleton :: Text -> a -> KeyMap a
objSingleton key = KeyMap.singleton (Key.fromText key)

#else

import Data.Aeson (Array, Object, Value (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

objDifference :: Object -> HashMap Text () -> Object
objDifference = HashMap.difference

objLookup :: Text -> Object -> Maybe Value
objLookup = HashMap.lookup

objNull :: Object -> Bool
objNull = HashMap.null

objSingleton :: Text -> a -> HashMap Text a
objSingleton = HashMap.singleton

#endif
