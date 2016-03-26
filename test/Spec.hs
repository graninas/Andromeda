{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, unless)
import Prelude hiding (init)

import Andromeda.LogicControl.Language

ship1 :: Profile
ship1 = undefined

online = BoolValue True

boostersHeatingUp :: Controller -> Script ()
boostersHeatingUp controller = do
    st <- ask controller status
    when (st == online) $ do
    
        t <- ask controller temperature
        report "temperature: " (show t)
        
        -- heating up
        command controller start
        command controller (power 1.0)
        t <- ask controller temperature
        report "temperature: " (show t)
        command controller stop
        
    unless (st == online)
        $ report "Failed to initialize boosters controller." ""

boostersTest :: Script ()
boostersTest = do
    load scheme ship1
    boosters <- get hardware "00:01" -- logical address in scheme
    controller <- init boosters
    boostersHeatingUp controller
    deinit controller


boostersMonitoring controller = do
    st <- ask controller status
    when (st == online) $ do
    -- TODO: what if controller goes offline here?
        t <- ask controller temperature
        -- TODO: this should be async.
        -- TODO: generic conversion to float depending on parameter type.
        save temperature (float t)
    unless (st == online)
        $ report "Controller goes offline." ""

boostersMonitoringTest :: Script ()
boostersMonitoringTest = do
    boosters <- get hardware "00:01"
    controller <- init boosters
    
    periodic 10.0 $ boostersMonitoring controller
    wait 105.0
    
    deinit controller


initialize = undefined
deinitialize = undefined
run = undefined
simulator = undefined




test1 :: IO ()
test1 = do
    -- validation, logging, event sourcing, fault tolerance, frp scenario

    let controlProgram = compile boostersMonitoringTest
    
    bracket (initialize simulator)
            (deinitialize)
            (\env -> run env controlProgram)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
