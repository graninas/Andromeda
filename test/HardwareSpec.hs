{-# LANGUAGE OverloadedStrings #-}
module HardwareSpec where

import Andromeda
import TestCommon

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Typeable
import Test.Hspec

spec = describe "Device runtime test." $ do
    it "Pure and impure device creators shoud return equal instances." $ do
        deviceIO <- makeDeviceIO boostersDef
        device1 <- readDeviceIO deviceIO
        let device2 = makeDevice boostersDef
        device1 `shouldBe` device2
    it "Reading of new device sensor should return 0." $ do
        deviceIO <- makeDeviceIO boostersDef
        device <- readDeviceIO deviceIO
        let t = readParameter nozzle1TCompIdx device
        t `shouldBe` (Just $ toKelvin 0.0)
    it "Setting and reading sensro value should be consistent." $ do
        let val = toKelvin 101.0
        deviceIO <- makeDeviceIO boostersDef
        setParameterIO deviceIO nozzle1TCompIdx val
        t <- readParameterIO nozzle1TCompIdx deviceIO
        t `shouldBe` (Just val)
        
    