{-# LANGUAGE OverloadedStrings #-}
module Main where

--import qualified BoostersHeatUpTest as T1

import Andromeda.LogicControl.Language
import Andromeda.Hardware.Language
import Andromeda.Common.Value

import Prelude hiding (read)
import Control.Monad.Free

fromKelvin :: Measurment Kelvin -> Float
fromKelvin (Measurment (FloatValue v)) = v
fromCelsius :: Measurment Celsius -> Float
fromCelsius (Measurment (FloatValue v)) = v

possible :: Controller -> Script Kelvin Float
possible controller = do
    t <- read controller temperature
    return $ fromKelvin t

{-
impossible :: Controller -> Script Celsius Float
impossible controller = do
    t <- read controller temperature
    return $ fromCelsius t
-}

main :: IO ()
main = do
    print "Testing..."
    --T1.test

