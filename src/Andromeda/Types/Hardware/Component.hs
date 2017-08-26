{-# LANGUAGE TemplateHaskell #-}
module Andromeda.Types.Hardware.Component where

import qualified Data.ByteString.Char8 as BS
import Control.Lens

import Andromeda.Types.Hardware.Types
import Andromeda.Types.Common.Guid

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
