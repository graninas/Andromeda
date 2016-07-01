{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}

module Andromeda.Language.External.LogicControl.Translator where

import Andromeda.Common
import Andromeda.LogicControl as LC
import Andromeda.Language.External.LogicControl.AST

import qualified Data.Map as M

type Arity = Int
type IsRet = Bool
data Constr where
    ContrScriptConstr :: String -> Arity -> IsRet -> Creator (ControllerScript a) -> Constr
    SysConstr :: String -> Arity -> Constr
  
type ConstructorsTable = M.Map String Constr
type ScriptsTable = M.Map ScriptType ConstructorsTable

type SysConstructorsTable = ConstructorsTable

data Creator scr where
   Arity0Cr :: scr -> Creator scr
   Arity1Cr :: Read arg1 => (arg1 -> scr) -> Creator scr
   Arity2Cr :: (Read arg1, Read arg2) => (arg1 -> arg2 -> scr) -> Creator scr
   Arity3Cr :: (Read arg1, Read arg2, Read arg3) => (arg1 -> arg2 -> arg3 -> scr) -> Creator scr

data Created = forall a. ContrScriptCreated (ControllerScript a)

ret = True
nret = False

fillControllerScriptConstrs :: ConstructorsTable
fillControllerScriptConstrs = M.fromList
    [ ("Get",  ContrScriptConstr "Get"  2 ret  (Arity2Cr LC.get))
    , ("Set",  ContrScriptConstr "Set"  3 nret (Arity3Cr LC.set))
    , ("Read", ContrScriptConstr "Read" 3 ret  (Arity3Cr LC.read))
    , ("Run",  ContrScriptConstr "Run"  2 ret  (Arity2Cr LC.run))
    ]

fillScriptsTable :: ScriptsTable
fillScriptsTable = M.fromList
    [ (ControllerScriptDef, fillControllerScriptConstrs)
    ]

fillSysConstructorsTable :: ConstructorsTable
fillSysConstructorsTable = M.fromList
    [ ("Controller", SysConstr "Controller" 1)
    , ("Command",    SysConstr "Command"    2)
    , ("Nothing",    SysConstr "Nothing"    0)
    ]

constructorArity (ContrScriptConstr _ arity _ _) = arity
constructorArity (SysConstr _ arity) = arity

createArgs [] = return []
createArgs (a:aas) = return (show a)

toArg (StringValue str) = Prelude.read str


feedControllerScript :: Creator (ControllerScript a) -> [Value] -> Created
feedControllerScript (Arity0Cr cr) []            = ContrScriptCreated $ cr
feedControllerScript (Arity1Cr cr) (a1:[])       = ContrScriptCreated $ cr (toArg a1)
feedControllerScript (Arity2Cr cr) (a1:a2:[])    = ContrScriptCreated $ cr (toArg a1) (toArg a2)
feedControllerScript (Arity3Cr cr) (a1:a2:a3:[]) = ContrScriptCreated $ cr (toArg a1) (toArg a2) (toArg a3)
feedControllerScript _ _ = error "feedControllerScript"

createFreeScript _ (ContrScriptConstr n a isRet cr) args = do
    aas <- createArgs args
    let c = feedControllerScript cr args    
    error "createFreeScript"
   
createFreeScript _ (SysConstr n a) args = do
    aas <- createArgs args
    
    
    let scr _ = "(" ++ n ++ " " ++ aas ++ ")"
    return scr