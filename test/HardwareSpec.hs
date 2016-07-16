{-# LANGUAGE OverloadedStrings #-}
module HardwareSpec where

import Andromeda
import TestCommon

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Typeable
import Test.Hspec

spec = describe "Hardware creation test" $ do
    it "makeDevice and makeDeviceIO shoud return equal instances" $ do
        deviceIO <- makeDeviceIO boostersDef
        device1 <- readDeviceIO deviceIO
        let device2 = makeDevice boostersDef
        device1 `shouldBe` device2
    it "read parameter should return value" $ do
        deviceIO <- makeDeviceIO boostersDef
        device <- readDeviceIO deviceIO
        let t = readParameter nozzle1T device
        t `shouldBe` (Just $ toKelvin 0.0)
    it "read and set (IO) parameter should return same value" $ do
        let val = toKelvin 101.0
        deviceIO <- makeDeviceIO boostersDef
        setParameterIO deviceIO nozzle1T val
        t <- readParameterIO nozzle1T deviceIO
        t `shouldBe` (Just val)