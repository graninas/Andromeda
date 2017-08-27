{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.ControllerScriptSpec where

import Prelude hiding (read)
import Control.Monad.Free
import qualified Data.ByteString.Char8 as BS
import Test.Hspec

import Test.Common

-- TODO: create new tests
-- ok (Right _) = True
-- ok _ = False
--
-- heatUp controller = do
--   -- t1 <- untag $ readTemperature controller nozzle2TCompIdx
--   t1 <- readTemperature controller nozzle2TCompIdx
--   res1 <- run controller start
--   if not . ok $ res1
--       then error $ show ("Scenario failed:", res1)
--       else do
--     res2 <- run controller (power 1.0)
--     if not . ok $ res2
--       then error $ show ("Scenario failed:", res2)
--       else do
--         res3 <- run controller stop
--         if not . ok $ res3
--           then error $ show ("Scenario failed:", res3)
--           else do
--             --t2 <- untag $ readTemperature controller nozzle2TCompIdx
--             t2 <- readTemperature controller nozzle2TCompIdx
--             return (t1, t2)
--
-- boostersHeatUp controller = do
--   st <- get controller status
--   if st == trueValue
--     then heatUp controller
--     else return (0.0, 0.0)

-- TODO: rework type Controller. Rework LogicControl.

spec = describe "Interpreting ControllerScript tests." $
  it "Interpreting ControllerScript tests."
    pending
  -- it "Running script should return values." $ do
  --   (vs, st) <- testInterpretControllerScript False $ boostersHeatUp (Controller "boosters")
  --   vs `shouldBe` (0.0, 0.0)
