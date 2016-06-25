{-# LANGUAGE Arrows #-}
module ReactiveArrowsTest where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import qualified Control.Monad.Free as F
import qualified Control.Monad.State as S
import System.Clock

import Andromeda
import TestCommon
import Lib

seconds n = fromNanoSecs $ n * 1000000000

pA :: ArrEffReact IO Int Int
pA = periodicAR (seconds 2)

mA :: ArrEffReact IO Int String
mA = mArr $ \b -> do
    liftIO $ print b
    return $ show b

periodicArr :: ArrEffReact IO Int String
periodicArr = pA >>> mA



runInfinitely n arrow st = do
    let st = runArrEff1 periodicArr n
    ((c, next), arrState) <- S.runStateT st emptyArrState
    print c
    runInfinitely (n + 1) next arrState
    
test :: IO ()
test = do
    print "ReactiveArrowsTest:"

    runInfinitely 1 periodicArr emptyArrState

    print "Done."
