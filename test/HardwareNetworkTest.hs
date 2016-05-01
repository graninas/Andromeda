{-# LANGUAGE OverloadedStrings #-}
module HardwareNetworkTest where

import Andromeda
import TestCommon

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS
import Data.Typeable

test = do
    print "HardwareNetworkTest:"
    
    hrdwIO <- makeHardwareIO boostersDef
    boostersInstance1 <- readHardwareIO hrdwIO

    let boostersInstance2 = makeHardware boostersDef
    if (boostersInstance1 == boostersInstance2)
        then print "SUCCESS"
        else print "FAIL"
    
    let t = readParameter "zone1-t" boostersInstance1
    print $ "zone1-t measurements: " ++ show t
    print "finished."
    
    
