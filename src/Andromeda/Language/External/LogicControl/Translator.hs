module Andromeda.Language.External.LogicControl.Translator where

import Andromeda.LogicControl
import Andromeda.Language.External.LogicControl.AST

import qualified Data.Map as M

type Arity = Int
type IsRet = Bool
data Constr = ScriptConstr String Arity IsRet
            | SysConstr String Arity
  deriving (Show, Read, Eq, Ord)
type ConstructorsTable = M.Map String Constr
type ScriptsTable = M.Map ScriptType ConstructorsTable

type SysConstructorsTable = ConstructorsTable

ret = True
nret = False

fillControllerScriptConstrs :: ConstructorsTable
fillControllerScriptConstrs = M.fromList
    [ ("Get", ScriptConstr "Get" 2 ret)
    , ("Set", ScriptConstr "Set" 3 nret)
    , ("Read", ScriptConstr "Read" 3 ret)
    , ("Run", ScriptConstr "Run" 2 ret)
    ]

fillScriptsTable :: ScriptsTable
fillScriptsTable = M.fromList
    [ (ControllerScriptDef, fillControllerScriptConstrs)
    ]

fillSysConstructorsTable :: ConstructorsTable
fillSysConstructorsTable = M.fromList
    [ ("Controller", SysConstr "Controller" 1)
    , ("Command", SysConstr "Command" 2)
    , ("Nothing", SysConstr "Nothing" 0)
--    , ("Run", ScriptConstr 2)
    ]

constructorArity (ScriptConstr _ arity _) = arity
constructorArity (SysConstr _ arity) = arity

createArgs [] = return []
createArgs (a:aas) = return (show a)

createFreeScript (ScriptConstr n a isRet) args = do
    aas <- createArgs args
    let scr next = "(" ++ n ++ " " ++ aas ++ next ++ ")"
    return scr
createFreeScript (SysConstr n a) args = do
    aas <- createArgs args
    let scr _ = "(" ++ n ++ " " ++ aas ++ ")"
    return scr