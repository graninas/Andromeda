{-# LANGUAGE OverloadedStrings #-}
module HardwareSpec where

import Andromeda
import TestCommon

import Test.Hspec

spec = describe "Device runtime test." $ do
    it "Pure and impure device creators should return equal instances." $ do
        deviceIO <- makeDeviceIO boostersDef "boosters"
        device1 <- readDeviceIO deviceIO
        let device2 = makeDevice boostersDef "boosters"
        device1 `shouldBe` device2
    it "Reading of new device sensor should return 0." $ do
        deviceIO <- makeDeviceIO boostersDef "boosters"
        device <- readDeviceIO deviceIO
        let t = readParameter nozzle1TCompIdx device
        t `shouldBe` (Just $ toKelvin 0.0)
    it "Setting and reading sensor value should be consistent." $ do
        let val = toKelvin 101.0
        deviceIO <- makeDeviceIO boostersDef "boosters"
        setParameterIO deviceIO nozzle1TCompIdx val
        t <- readParameterIO nozzle1TCompIdx deviceIO
        t `shouldBe` (Just val)
    it "Getting deviceId" $ do
        deviceIO <- makeDeviceIO boostersDef "boosters"
        device <- readDeviceIO deviceIO
        let dId = getDeviceId device
        dId `shouldBe` "boosters"
    