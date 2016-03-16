{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, unless)
import Prelude hiding (init)

import Andromeda.HAL.Language
import Andromeda.HAL.Interpreter

ship1 :: Profile
ship1 = undefined

online = BoolValue True

boostersHeatingUp :: Script ()
boostersHeatingUp = do
    
    load scheme ship1
    boosters <- get hardware "00:01" -- physical address in scheme

    controller <- init boosters
    st <- ask controller status
    when (st == online) $ do
    
        t <- ask controller temperature
        report "temperature: " (show t)
        
        command controller start
        command controller (power 1.0)
        t <- ask controller temperature
        report "temperature: " (show t)
        
        command controller stop
    unless (st == online)
        $ report "Failed to initialize boosters controller." ""

boostersTemperatureMonitoring :: Script ()
boostersTemperatureMonitoring = do
    boosters <- get hardware "00:01"
    controller <- init boosters
    monitoring controller
  where monitoring = do
    st <- ask controller status
        when (st == online) $ do
            t <- ask controller temperature

            -- TODO: this should be async.
            -- TODO: generic conversion to float depending on parameter type.
            save temperature (float t)
            wait 10.0
            monitoring
        unless (st == online)
            $ report "Controller goes offline." ""
        

        
initializeEnvironment = undefined
deinitializeEnvironment = undefined
run = undefined

test1 :: IO ()
test1 = do
    -- validation, logging, event sourcing, fault tolerance, frp scenario
    let controlProgram = compile [
        boostersHeatingUp,
        boostersTemperatureMonitoring
        ]

    initializeEnvironment
    run controlProgram
    deinitializeEnvironment
    

main :: IO ()
main = putStrLn "Test suite not yet implemented"
