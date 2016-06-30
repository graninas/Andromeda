module Andromeda.Language.External.LogicControl.Translator where

import Andromeda.LogicControl
import Andromeda.Language.External.LogicControl.AST

import qualified Data.Map as M

type Arity = Int
data Constr = ScriptConstr Arity
            | SysConstr Arity
  deriving (Show, Read, Eq, Ord)
type ConstructorsTable = M.Map String Constr
type ScriptsTable = M.Map ScriptType ConstructorsTable

type SysConstructorsTable = ConstructorsTable


fillControllerScriptConstrs :: ConstructorsTable
fillControllerScriptConstrs = M.fromList
    [ ("Get", ScriptConstr 2)
    , ("Set", ScriptConstr 3)
    , ("Read", ScriptConstr 3)
    , ("Run", ScriptConstr 2)
    ]

fillScriptsTable :: ScriptsTable
fillScriptsTable = M.fromList
    [ (ControllerScriptDef, fillControllerScriptConstrs)
    ]

fillSysConstructorsTable :: ConstructorsTable
fillSysConstructorsTable = M.fromList
    [ ("Controller", SysConstr 1)
    , ("Command", SysConstr 2)
--    , ("Read", ScriptConstr 3)
--    , ("Run", ScriptConstr 2)
    ]

constructorArity (ScriptConstr arity) = arity
constructorArity (SysConstr arity) = arity



