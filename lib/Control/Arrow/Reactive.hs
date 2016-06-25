{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}
module Control.Arrow.Reactive where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad
import Control.Arrow.ArrEff
import qualified Control.Monad.State as S
import System.Clock

periodicA' :: MonadIO eff => TimeSpec -> TimeSpec -> ArrEff eff b c -> b -> eff (Prod eff b c)
periodicA' dt ts arrow b = do
    ts' <- liftIO $ getTime Monotonic
    let dts = diffTimeSpec ts ts'
    if (dts < dt)
       then periodicA' dt ts arrow b
       else do
           liftIO $ print dts
           runArrEff1 arrow b

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
        
        

-- Netwire-like FRP arrows (but not exactly)
{-
data ArrEffReact eff b c = ArrEffReact (b -> ArrStateEff eff (c, ArrEffReact eff b c))

instance Monad eff => Category (ArrEffReact eff) where
    id = ArrEffReact (\b -> return (b, id))
    ArrEffReact g . ArrEffReact f = ArrEffReact arrFG
      where
        arrFG a = do
            fa <- f a
            feededF fa
        feededF (b, arr1) = do
            gb <- g b
            feededG arr1 gb
        feededG arr1 (c, arr2) = return (c, arr2 . arr1)

instance Monad eff => Arrow (ArrEffReact eff) where
    arr f = ArrEffReact (\b -> return (f b, arr f))
    first (ArrEffReact f) = ArrEffReact arrF
      where
        arrF (b, d) = do
            fb <- f b
            feededF fb d
        feededF (c, arr1) d = return ((c, d), first arr1)

instance Monad eff => Functor (ArrEffReact eff b) where
    fmap f (ArrEffReact r) = ArrEffReact (\b -> do
        (c, next) <- r b
        return (f c, fmap f next))

mArrR mf = ArrEffReact $ \b -> do
    c <- mf b
    return (c, mArrR mf)
-}
data Produce c = Inhibit | Produce c
data ArrState = forall c. ArrState (Produce c) TimeSpec
type ArrStateEff eff = S.StateT ArrState eff

emptyArrState = ArrState Inhibit (fromNanoSecs 0)

type ArrEffReact eff b c = ArrEff (ArrStateEff eff) b c



(-->) :: Monad eff => ArrEffReact eff b c -> ArrEffReact eff b c -> ArrEffReact eff b c
(ArrEff f) --> arr2@(ArrEff g) = ArrEff $ \b -> do
    (c, arr1') <- f b
    producing <- checkProducing
    if producing
       then return (c, arr1' --> arr2)
       else runArrEff1 arr2 b

infixr 1 -->

-- Will leap on long computations and small dts
periodicAR :: MonadIO eff => TimeSpec -> ArrEffReact eff b b
periodicAR dt = mArr $ \b -> do
    prevT <- getPrevTime
    curT <- liftIO $ getTime Monotonic
    if (diffTimeSpec curT prevT >= dt)
       then do
           let nextT = calcNextTime dt prevT curT
           produceWithT b nextT
       else inhibitWith b


getPrevTime :: Monad eff => ArrStateEff eff TimeSpec
getPrevTime = do
    (ArrState _ prevT) <- S.get
    return prevT

calcNextTime :: TimeSpec -> TimeSpec -> TimeSpec -> TimeSpec
calcNextTime dt prevT curT =
    let nextTimes = iterate (+dt) prevT
    in  head $ dropWhile (<curT) nextTimes

produceWithT :: Monad eff => b -> TimeSpec -> ArrStateEff eff b
produceWithT b ts = do
    S.put $ ArrState (Produce b) ts
    return b

inhibitWith :: Monad eff => b -> ArrStateEff eff b
inhibitWith b = do
    ArrState _ ts <- S.get
    S.put $ ArrState Inhibit ts
    return b

checkProducing :: Monad eff => ArrStateEff eff Bool
checkProducing = do
    ArrState p _ <- S.get
    case p of
         Inhibit -> return False
         _ -> return True