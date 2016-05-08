{-# LANGUAGE OverloadedStrings #-}

module Andromeda.Assets.Hardware.Components where

import Andromeda.Hardware.Description

aaaManufacturer = "AAA Inc."
guid1 = "3539390d-f189-4434-bd9e-d39e494a869a"
guid2 = "c80a1ba3-1e7a-4af9-a10b-2313cc10f9e4"
guid3 = "02ecabc3-1a02-481f-8e9a-7b0cc62e38f5"

aaa_p_02   = component sensors       guid1 aaaManufacturer "Pressure sensor AAA-P-02"
aaa_t_25   = component sensors       guid2 aaaManufacturer "Temperature sensor AAA-T-25"
aaa_controller_01 = component terminalUnits guid3 aaaManufacturer "Controller AAA-C-01"


