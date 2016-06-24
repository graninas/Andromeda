{-# LANGUAGE Arrows #-}
module Control.Arrow.Reactive where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Arrow.ArrEff
import System.Clock

periodicA' :: MonadIO eff => TimeSpec -> TimeSpec -> ArrEff eff b c -> b -> eff (Prod eff b c)
periodicA' dt ts arrow b = do
    ts' <- liftIO $ getTime Monotonic
    let dts = diffTimeSpec ts ts'
    if (dts < dt)
       then periodicA' dt ts arrow b
       else runArrEff1 arrow b
    
periodicA :: MonadIO eff => Int -> ArrEff eff b c -> ArrEff eff b c
periodicA dt arrow = 
    let ns = fromNanoSecs $ fromIntegral dt
    in mArr $ \b -> do
        (c, _) <- periodicA' ns (fromNanoSecs 0) arrow b
        return c




runArrEvent :: Read b => ArrEff IO b (Bool, c) -> [c] -> IO [c]
runArrEvent (ArrEff f) cs = do
    b <- getLine
    result <- f (read b)
    case result of
        ((True, c), next) -> runArrEvent next (c:cs)
        ((False, c), _)   -> return (c:cs)