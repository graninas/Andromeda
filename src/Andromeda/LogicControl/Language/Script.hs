{-# LANGUAGE Rank2Types #-}
module Andromeda.LogicControl.Language.Script where

import Andromeda.LogicControl.Language.Controller


data Script b = ControllerScript (ControllerScript b)
              | ComputationScript
