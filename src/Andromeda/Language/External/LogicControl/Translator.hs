{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
--{-# LANGUAGE ExistentialQuantification #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE ImpredicativeTypes #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

module Andromeda.Language.External.LogicControl.Translator where

import Andromeda.Common
import Andromeda.LogicControl as LC hiding (read)
import qualified Andromeda.LogicControl as LC (read)
import Andromeda.Language.External.LogicControl.AST

import Control.Lens hiding (getConst)
import Control.Monad.Trans.State as S
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe

type Arity = Int
data Constr where
    ContrScriptConstr :: (Show a, Read a) => String -> Arity -> Creator (ControllerScript a) -> Constr
    SysConstr         :: (Show a, Read a) => String -> Arity -> Creator a -> Constr
  
type ConstructorsTable = M.Map String Constr
type ScriptsDefsTable = M.Map ScriptType ConstructorsTable

type SysConstructorsTable = ConstructorsTable

data Creator scr where
   Arity0Cr :: scr -> Creator scr
   Arity1Cr :: Read arg1 => (arg1 -> scr) -> Creator scr
   Arity2Cr :: (Read arg1, Read arg2) => (arg1 -> arg2 -> scr) -> Creator scr
   Arity3Cr :: (Read arg1, Read arg2, Read arg3) => (arg1 -> arg2 -> arg3 -> scr) -> Creator scr

data Created where
    CreatedControllerScript :: Show a => (ControllerScript a) -> Created
    CreatedConstr :: (Read a, Show a) => a -> Created
    CreatedConst :: Constant -> Created
    CreatedArgs :: [Created] -> Created

data Composed where
    ComposedVal :: IdName -> Created -> Composed
    
data ScriptResolved where
    ContrScriptResolved :: ControllerScript () -> ScriptResolved
    
type Table = (String, M.Map String String)
type ScriptsTable = M.Map IdName (Script ())

data Tables = Tables {
      _constants :: Table
    , _values :: Table
    , _scriptDefs :: ScriptsDefsTable
    , _sysConstructors :: SysConstructorsTable
    , _scripts :: ScriptsTable
}

data Translator = Translator {
      _tables :: Tables
    , _controlProg :: ControlProgram ()
    , _scriptTranslation :: Maybe ScriptType
    , _indentation :: Int
    , _printIndentation :: Int
    , _uniqueNumber :: Int
}

makeLenses ''Tables
makeLenses ''Translator

type TranslatorSt a = S.StateT Translator IO a

incPrintIndentation, decPrintIndentation :: TranslatorSt ()
incPrintIndentation = printIndentation += 1
decPrintIndentation = printIndentation -= 1

print' :: String -> TranslatorSt ()
print' s = do
    i <- use printIndentation
    liftIO $ putStr $ replicate (i * 4) ' '
    liftIO $ putStrLn s

fillControllerScriptConstrs :: ConstructorsTable
fillControllerScriptConstrs = M.fromList
    [ ("Get",  ContrScriptConstr "Get"  2 (Arity2Cr LC.get))
    , ("Set",  ContrScriptConstr "Set"  3 (Arity3Cr LC.set))
    , ("Read", ContrScriptConstr "Read" 3 (Arity3Cr LC.read))
    , ("Run",  ContrScriptConstr "Run"  2 (Arity2Cr LC.run))
    ]

fillScriptsDefsTable :: ScriptsDefsTable
fillScriptsDefsTable = M.fromList
    [ (ControllerScriptDef, fillControllerScriptConstrs)
    ]

fillSysConstructorsTable :: ConstructorsTable
fillSysConstructorsTable = M.fromList
    [ ("Controller", SysConstr "Controller" 1 (Arity1Cr Controller))
    , ("Command",    SysConstr "Command"    1 (Arity1Cr Command))
    ]

constructorArity (ContrScriptConstr _ arity _) = arity
constructorArity (SysConstr _ arity _) = arity

printCreated (CreatedConstr a) = do
    print' $ "Created Constr "
printCreated (CreatedControllerScript a) = do
    print' $ "Created Constr Script "
printCreated (CreatedConst c) = do
    print' $ "Created const: "
    incPrintIndentation
    print' $ show c
    decPrintIndentation
printCreated (CreatedArgs []) = print' $ "Created args: none"
printCreated (CreatedArgs args) = do
    print' $ "Created args: "
    incPrintIndentation
    mapM_ printCreated args
    decPrintIndentation

feedControllerCreator :: Creator (ControllerScript a) -> [String] -> ControllerScript a
feedControllerCreator (Arity0Cr cr) []            = cr
feedControllerCreator (Arity1Cr cr) (a1:[])       = cr (read a1)
feedControllerCreator (Arity2Cr cr) (a1:a2:[])    = cr (read a1) (read a2)
feedControllerCreator (Arity3Cr cr) (a1:a2:a3:[]) = cr (read a1) (read a2) (read a3)
feedControllerCreator _ _ = error "feedControllerCreator"

feedCreator :: Creator a -> [String] -> a
feedCreator (Arity0Cr cr) []            = cr
feedCreator (Arity1Cr cr) (a1:[])       = cr (read a1)
feedCreator (Arity2Cr cr) (a1:a2:[])    = cr (read a1) (read a2)
feedCreator (Arity3Cr cr) (a1:a2:a3:[]) = cr (read a1) (read a2) (read a3)
feedCreator _ _ = error "feedCreator"

createArg (CreatedConst (StringConstant str)) = show str
createArg (CreatedConst (IntegerConstant i))  = show i
createArg (CreatedConstr c) = show c

createConstructor _ (ContrScriptConstr n a cr) ca@(CreatedArgs args) = do
    print' $ "createConstructor ContrScriptConstr " ++ n
    printCreated ca
    let aas = map createArg args
    let scr = feedControllerCreator cr aas
    return $ CreatedControllerScript scr
    
createConstructor _ (SysConstr n a cr) ca@(CreatedArgs args) = do
    print' $ "createConstructor SysConstr " ++ n
    printCreated ca
    let aas = map createArg args
    let c = feedCreator cr aas
    return $ CreatedConstr c
