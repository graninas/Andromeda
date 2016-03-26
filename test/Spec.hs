{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, unless)
import Prelude hiding (init)

import Andromeda.LogicControl.Language
import Andromeda.Environment.Language

shipScheme :: Scheme
shipScheme = undefined

online = (boolValue True ==)
stop = undefined
start = undefined
power = undefined

sendData :: Data -> Script ()
sendData = sendTo [database, reporter]
sendData :: String -> Script ()
sendReport = sendTo [reporter] . info

-- TODO
temperature = undefined

-- We see a lot of boilerplate with Controller. Can we move it to background? Reader monad? 'with controller $ do'
boostersHeatingUp :: Controller -> Script ()
boostersHeatingUp controller = do
    st <- ask controller status
    when (online st) $ do
    
        -- this is a bit verbose. too many operations. Can we bind temperature read to receiver slot? (Reactive)
        t <- read controller temperature
        sendData t

        -- heating up
        run controller start
        run controller (power 1.0)
        
        ask controller temperature
            >>= sendData
        
        command controller stop
        
    unless (online st)
        $ sendReport "Boosters controller is offline."

boostersTest :: ControlProgram ()
boostersTest = do
    load shipScheme
    log "Ship scheme is loaded.
    with (hardware "00:01") -- With multiple devices? Hardware returns [Controller]?
        $ \controller -> eval once $ boostersHeatingUp controller
    log "Boosters test done."


boostersMonitoring controller = do
    st <- ask controller status
    when (st == online) $ do
        t <- ask controller temperature
        save temperature (float t)
    unless (st == online)
        $ report "Controller goes offline." ""

boostersMonitoringTest :: Script ()
boostersMonitoringTest = do
    boosters <- get hardware "00:01"
    controller <- init boosters
    eval (times 5 10.0) $ boostersMonitoring controller
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
