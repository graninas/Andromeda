{-# LANGUAGE Rank2Types #-}
module Andromeda.Types.Language.Scripting.ScriptWrapper where

import Andromeda.Types.Language.Scripting.ControllerScript
import Andromeda.Types.Language.Scripting.ComputationScript
import Andromeda.Types.Language.Scripting.InfrastructureScript

-- TODO: ScriptWrapper can be replaced by existential types / rankN types.
data ScriptWrapper b
  = ControllerScriptWrapper (ControllerScript b)
  | ComputationScriptWrapper (ComputationScript b)
  | InfrastructureScriptWrapper (InfrastructureScript b)

controllerScriptWrapper :: ControllerScript b -> ScriptWrapper b
controllerScriptWrapper = ControllerScriptWrapper

computationScriptWrapper :: ComputationScript b -> ScriptWrapper b
computationScriptWrapper = ComputationScriptWrapper

infrastructureScriptWrapper :: InfrastructureScript b -> ScriptWrapper b
infrastructureScriptWrapper = InfrastructureScriptWrapper
