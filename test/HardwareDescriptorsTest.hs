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
    if (boostersInstance1 == boostersInstance2)
        then print "SUCCESS"
        else print "FAIL"
    
    let t = readParameter nozzle1T boostersInstance1
    print $ (BS.unpack nozzle1T) ++ " measurements: " ++ show t
    print "finished."
    
    
