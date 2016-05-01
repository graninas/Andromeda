{-# LANGUAGE OverloadedStrings #-}

module Andromeda.Assets.Hardware.Devices where

import Andromeda.Hardware.Description

aaaManufacturer = "AAA Inc."

aaa_p_02   = device Sensors "3539390d-f189-4434-bd9e-d39e494a869a" "Pressure sensor AAA-P-02"        aaaManufacturer
aaa_t_25   = device Sensors "c80a1ba3-1e7a-4af9-a10b-2313cc10f9e4" "Temperature sensor AAA-T-25"     aaaManufacturer
aaa_rtu_01 = device Rtus    "02ecabc3-1a02-481f-8e9a-7b0cc62e38f5" "Remote terminal unit AAA-RTU-01" aaaManufacturer


