{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HardwareDescriptorsTest where

import Andromeda
import TestCommon

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

boostersDescriptor :: Hdl ()
boostersDescriptor = do
    sensor aaa_t_25 "zone1-t" temperaturePar
    sensor aaa_p_02 "zone1-p" pressurePar
    sensor aaa_p_02 "zone2-p" pressurePar


test = do
    print "HardwareDescriptorsTest:"
    
    boosters1 <- makeHardwareIO boostersDescriptor
    h <- readHardwareIO boosters1
    print h
    
    let t = readParameter "zone1-t" h 
    print t
    print "finished."
    
    

f x  = case cast x of
                  Just (x :: Char) -> show x
                  Nothing -> "unknown"
                  
f2 tr = case cast tr of
    Just (v1 :: Measurement Kelvin) -> print "a"
    Nothing -> case cast tr of
        Just (v2 :: Measurement Pascal) -> print "b"
        Nothing -> case cast tr of
            Just (v2 :: Measurement Celsius) -> print "c"
            Nothing -> error "bad cast"
            
            
