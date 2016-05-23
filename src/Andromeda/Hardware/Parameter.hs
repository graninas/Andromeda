{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Andromeda.Hardware.Parameter where

import Andromeda.Calculations
import Andromeda.Common

import Data.Typeable

-- First attempt (used in many modules)
data Parameter tag = Temperature | Pressure
  deriving (Show, Read, Eq)

data Power -- 'Power' units for boosters...

-- Idea: http://stackoverflow.com/questions/28243383/how-can-i-read-the-metadata-of-a-type-at-runtime
data Admissible a where
    AdKelvin  :: Admissible Kelvin
    AdCelsius :: Admissible Celsius
    AdPascal  :: Admissible Pascal

convertAdmissible :: Admissible a -> a -> Value -> Measurement a
convertAdmissible AdKelvin  m v = toKelvinV v
convertAdmissible AdCelsius m v = toCelsiusV v
convertAdmissible AdPascal  m v = toPascalV v

data Measurementable a where
   MeasureKelvin :: Measurementable Kelvin
   MeasureCelsius :: Measurementable Celsius
   MeasurePascal :: Measurementable Pascal



-- TODO: think how to do it
--toMeasurement :: TypeRep -> Value -> forall a. Measurement a
toMeasurement (Par v m) = case cast m of
    Just (m1 :: Measurement Kelvin) -> toMeasurementV v
    Nothing -> case cast m of
        Just (m2 :: Measurement Pascal) -> toMeasurementV v
        Nothing -> case cast m of
            Just (m3 :: Measurement Celsius) -> toMeasurementV v
            Nothing -> error "bad cast"
-- or it:
--toMeasurement :: DataType -> Value -> Measurement a
--toMeasurement dt v = undefined

toPower :: Int -> Measurement Power
toPower v = Measurement (intValue v)

temperature :: Parameter Kelvin
temperature = Temperature
pressure :: Parameter Pascal
pressure = Pressure

temperatureKelvin :: Parameter Kelvin
temperatureKelvin = temperature
temperatureCelsius :: Parameter Celsius
temperatureCelsius = Temperature


-- Second attempt (used in HDL)
data Par = forall tag. Typeable tag => Par Value (Measurement tag)

instance Show Par where
  show (Par v t) = "Par v:" ++ show v ++ " t:" ++ show t
  
instance Eq Par where
  (Par v1 t1) == (Par v2 t2) = v1 == v2

temperaturePar = Par (toValue zeroKelvin) zeroKelvin
pressurePar    = Par (toValue zeroPascal) zeroPascal

toPar :: Typeable tag => Measurement tag -> Par
toPar m@(Measurement v) = Par v m 

