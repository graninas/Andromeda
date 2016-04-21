{-# LANGUAGE OverloadedStrings #-}

module HardwareDescriptorsTest where

import Andromeda
import TestCommon

import Data.IORef
import Data.Typeable
import Data.Data

--boostersDescriptor :: Hdl ()
boostersDescriptor = do
    sensor aaa_t_25 "zone1-t" temperature
    untag $ sensor aaa_p_02 "zone1-p" pressure
    untag $ sensor aaa_p_02 "zone2-p" pressure


test = do
    print "HardwareDescriptorsTest:"
    
    boosters1 <- makeHardware boostersDescriptor
    h <- readIORef boosters1
    print $ typeOf h
    
    --t <- readParam "zone1-t" :: IO (Measurment Kelvin)
    --print t
    print "finished."
