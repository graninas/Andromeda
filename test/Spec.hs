module Test where

import Andromeda.HAL

ship1 :: Scheme
ship1 = undefined


boosters10Percent :: Script
boosters10Percent = do
    
    load scheme ship1
    
    boosters <- get hardware "00:01" -- physical address in scheme
    validate $ do
        equal (boosters.name)         b12
        equal (boosters.manufacturer) b12_manufacturer

    controller <- init boosters
    st <- ask controller status
    validate $ do
        equal (st.value) online
    
    t <- ask controller temperature
    validate $ do
        less    (t.value) 300
        greater (t.value) 0
    
    run command controller start
    run command controller (power 10)

    t <- ask controller temperature
    validate $ do
        less    (t.value) 1200
        greater (t.value) 0
        
    run command controller stop
    

initializeEnvironment = undefined
deinitializeEnvironment = undefined
run = undefined
compile = undefined

test1 = do
    -- validation, logging, event sourcing, fault tolerance, frp scenario
    let controlProgram = compile boosters10Percent

    initializeEnvironment
    run controlProgram
    deinitializeEnvironment
    

main :: IO ()
main = putStrLn "Test suite not yet implemented"
