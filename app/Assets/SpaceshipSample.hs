{-# LANGUAGE OverloadedStrings #-}
module Assets.SpaceshipSample where

import Andromeda.Types.Hardware
import Andromeda.Types.Language.Hardware

aaaManufacturer = "AAA Inc."
guid1 = "3539390d-f189-4434-bd9e-d39e494a869a"
guid2 = "c80a1ba3-1e7a-4af9-a10b-2313cc10f9e4"
guid3 = "02ecabc3-1a02-481f-8e9a-7b0cc62e38f5"

aaa_p_02 = component sensors guid1 aaaManufacturer "Pressure sensor AAA-P-02"
aaa_t_25 = component sensors guid2 aaaManufacturer "Temperature sensor AAA-T-25"
aaa_controller_01 = component controllers guid3 aaaManufacturer "Controller AAA-C-01"

nozzleTemeratureCompIdx, nozzlePressureCompIdx :: ComponentIndex
nozzle1TCompIdx, nozzle2TCompIdx, nozzle1PCompIdx, nozzle2PCompIdx :: ComponentIndex
rotaryEngineControllerCompIdx, boostersControllerCompIdx :: ComponentIndex
nozzleTemeratureCompIdx = "nozzle-t"
nozzlePressureCompIdx = "nozzle-p"
nozzle1TCompIdx = "nozzle1-t"
nozzle2TCompIdx = "nozzle2-t"
nozzle1PCompIdx = "nozzle1-p"
nozzle2PCompIdx = "nozzle2-P"
rotaryEngineControllerCompIdx = "rotary-engine-controller"
boostersControllerCompIdx = "boosters-controller"

boostersAddr :: PhysicalAddress
boostersAddr = "01:02" -- (row, column)
boostersTUAddr :: PhysicalAddress
boostersTUAddr = "03:02" -- (row, column)

boostersNozzle1T   = (boostersAddr, nozzle1TCompIdx)
boostersNozzle1P   = (boostersAddr, nozzle1PCompIdx)
boostersNozzle2T   = (boostersAddr, nozzle2TCompIdx)
boostersNozzle2P   = (boostersAddr, nozzle2PCompIdx)
boostersController = (boostersAddr, boostersControllerCompIdx)

boostersDef :: Hdl ()
boostersDef = do
    sensor aaa_t_25 nozzle1TCompIdx temperaturePar
    sensor aaa_t_25 nozzle2TCompIdx temperaturePar
    sensor aaa_p_02 nozzle1PCompIdx pressurePar
    sensor aaa_p_02 nozzle2PCompIdx pressurePar
    controller aaa_controller_01 boostersControllerCompIdx

rotaryEngineDef :: Hdl ()
rotaryEngineDef = do
    sensor aaa_t_25 nozzleTemeratureCompIdx temperaturePar
    sensor aaa_p_02 nozzlePressureCompIdx   pressurePar
    controller aaa_controller_01 rotaryEngineControllerCompIdx

networkDef :: Hndl ()
networkDef = do
    iBoosters <- remoteDevice boostersAddr boostersDef "boosters"
    iBoostersTU <- terminalUnit boostersTUAddr "boosters terminal unit"
    linkedDevice iBoosters iBoostersTU
    iLogicControl <- logicControl "08:02" "main logic control"
    link iLogicControl iBoostersTU

-- dummy
getDeviceDefs =
    [ makeDevice boostersDef "boosters"
    , makeDevice rotaryEngineDef "rotary engine" ]
