{-# LANGUAGE Rank2Types #-}
module Andromeda.Types.Language.Scripting.ScriptWrapper where

import Andromeda.Types.Language.Scripting.ControllerScript
import Andromeda.Types.Language.Scripting.ComputationScript
import Andromeda.Types.Language.Scripting.InfrastructureScript

-- TODO: ScriptWrapper can be replaced by existential types / rankN types.
data ScriptWrapper b
  = ControllerWrapper (ControllerScript b)
  | ComputationWrapper (ComputationScript b)
  | InfrastructureWrapper (InfrastructureScript b)

infrastructureWrapper :: InfrastructureScript b -> ScriptWrapper b
infrastructureWrapper = InfrastructureWrapper

controllerWrapper :: ControllerScript b -> ScriptWrapper b
controllerWrapper = ControllerWrapper

computationWrapper :: ComputationScript b -> ScriptWrapper b
computationWrapper = ComputationWrapper
