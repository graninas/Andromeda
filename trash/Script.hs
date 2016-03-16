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
        less    (t.value) 300.0
        greater (t.value) 0.0
    
    run command controller start
    run command controller (power 10.0)

    t <- ask controller temperature
    validate $ do
        less    (t.value) 1200.0
        greater (t.value) 0.0
        
    run command controller stop
