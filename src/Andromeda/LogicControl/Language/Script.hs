{-# LANGUAGE Rank2Types #-}
module Andromeda.LogicControl.Language.Script where

import Andromeda.LogicControl.Language.Controller
import Andromeda.LogicControl.Language.Computation
import Andromeda.LogicControl.Language.Infrastructure

data Script b = ControllerScript (ControllerScript b)
              | ComputationScript (ComputationScript b)
              | InfrastructureScript (InfrastructureScript b)

infrastructureScript :: InfrastructureScript b -> Script b
infrastructureScript = InfrastructureScript

controllerScript :: ControllerScript b -> Script b
controllerScript = ControllerScript

computationScript :: ComputationScript b -> Script b
computationScript = ComputationScript