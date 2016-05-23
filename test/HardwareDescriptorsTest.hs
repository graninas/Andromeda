{-# LANGUAGE OverloadedStrings #-}
module HardwareDescriptorsTest where

import Andromeda
import TestCommon

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

test = do
    print "HardwareDescriptorsTest:"
    
    hrdwIO <- makeDeviceIO boostersDef
    boostersInstance1 <- readDeviceIO hrdwIO

    let boostersInstance2 = makeDevice boostersDef
    print $ if boostersInstance1 == boostersInstance2
            then "SUCCESS"
            else "FAIL"
    
    let t = readParameter nozzle1T boostersInstance1
    print $ BS.unpack nozzle1T ++ " measurements: " ++ show t
    
    setParameterIO hrdwIO nozzle1T (toKelvin 101.0)
    mbt2 <- readParameterIO nozzle1T hrdwIO
    case mbt2 of
        Just t2 -> if toKelvin 101.0 == t2 
                   then print "setParamIO success"
                   else print "setParamIO Fail" >> print t2
        Nothing -> print "Fail: no result"
    
    
    print "finished."
    
    
