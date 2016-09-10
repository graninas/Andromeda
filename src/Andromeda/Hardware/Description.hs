{-# LANGUAGE TemplateHaskell #-}
module Andromeda.Hardware.Description where

import Andromeda.Common.Guid

import qualified Data.ByteString.Char8 as BS
import Control.Lens

type HardwareName = BS.ByteString
type Description = BS.ByteString

data ComponentClass
    = Sensors
    | Controllers
  deriving (Show, Read, Eq)

-- | Class, manufacturer, description, serial number.
-- Just info.
data ComponentDef = ComponentDef
    { _componentClass :: ComponentClass
    , _componentGuid  :: Guid 
    , _componentManufacturer :: BS.ByteString
    , _componentDescritpion  :: HardwareName }
  deriving (Show, Read, Eq)

makeLenses ''ComponentDef

component = ComponentDef

sensors = Sensors
controllers = Controllers
