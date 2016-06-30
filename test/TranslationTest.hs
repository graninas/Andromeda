{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
module TranslationTest where

import Andromeda
import TestCommon
import Paths_Andromeda

import Control.Lens hiding (getConst)
import Control.Monad.Trans.State as S
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe

type Table = (String, M.Map String String)

data Tables = Tables {
      _constants :: Table
    , _values :: Table
    , _scripts :: ScriptsTable
    , _sysConstructors :: SysConstructorsTable
}

data Translator = Translator {
      _tables :: Tables
    , _controlProg :: ControlProgram ()
    , _scriptTranslation :: Maybe ScriptType
    , _indentation :: Int
}

makeLenses ''Tables
makeLenses ''Translator

type TranslatorSt a = S.StateT Translator IO a

print' :: String -> TranslatorSt ()
print' s = liftIO $ print s

enableScriptTranslation st = do
    assertNoScriptTranslation
    assign scriptTranslation (Just st)
disableScriptTranslation = assign scriptTranslation Nothing
isScriptTranslation :: TranslatorSt Bool
isScriptTranslation = do
    mbst <- use scriptTranslation
    return $ isJust mbst
    
setIndentation n = assign indentation n
incIndentation, decIndentation :: TranslatorSt ()
incIndentation = indentation += 1
decIndentation = do
    assertIndentation (>0)
    indentation -= 1
assertIndentation p = do
    i <- use indentation
    assert (p i) "wrong indentation:" i

assert False msg n = error $ msg ++ " " ++ show n
assert _ _ _ = return ()    
    
getConst :: IdName -> TranslatorSt (Maybe String)
getConst n = use (tables . constants . _2 . at n)

assertNoScriptTranslation = do
    x <- use scriptTranslation
    assert (isNothing x) "script translation enabled:" x

assertNotExistIn :: Lens' Tables Table  -> IdName -> TranslatorSt ()
assertNotExistIn table key = do
    n <- use $ tables . table . _1
    x <- use $ tables . table . _2 . at key
    assert (isNothing x) (" exist: " ++ key ++ " for ") n
    
translateExpr = error "translateExpr"


runLIStatements [] = return ()
runLIStatements (LinedEmptyStmt:stmts) = runLIStatements stmts
runLIStatements (LinedIndentedStmt (IndentedStmt i stmt):stmts) = do
    assertIndentation (==i)
    runStatement stmt

{-
data Statement = ConstantStmt IdName Expr
               | ValStmt IdName Expr
               | CallStmt Expr
               
data Expr = ConstantExpr Constant
          | ConstructorExpr Constructor
          | IdentifierExpr Identifier
  deriving (Show)
-}

findScriptConstructor :: IdName -> ScriptType -> TranslatorSt (Maybe Constr)
findScriptConstructor n st = do
    mbs <- use (tables . scripts . at st)
    case mbs of
         Nothing -> return Nothing
         Just t -> return $ view (at n) t

findSysConstructor :: IdName -> TranslatorSt (Maybe Constr)
findSysConstructor n = use (tables . sysConstructors . at n)
         
findProcedure _ = return Nothing -- TODO

findConstructor :: IdName -> TranslatorSt (Maybe Constr)
findConstructor n = do
    msc <- use scriptTranslation
    mbScrC <- maybe (return Nothing) (findScriptConstructor n) msc
    mbSysC <- findSysConstructor n
    mbProc <- findProcedure n
    case () of
         _ | isJust mbScrC -> return mbScrC
         _ | isJust mbSysC -> return mbSysC
         _ | isJust mbProc -> return mbProc
         _                 -> return Nothing

translateArgs NoneArgs  = return []
translateArgs (Args es) = mapM translateExpression es

runValueStatement name expr = do
    print' $ "run value stmt" ++ name
    assertNotExistIn values name
    r <- runScriptExpression expr
    
    error "TODO: runValueStatement"
    return ()

runScriptExpression :: Expr -> TranslatorSt Value
runScriptExpression (ConstructorExpr c) = runConstructor c
runScriptExpression (ConstantExpr c)    = runConstant c
runScriptExpression _ = error "translateExpression"

runConstant :: Constant -> TranslatorSt Value
runConstant (StringConstant str) = return $ StringValue str
runConstant (IntegerConstant i) = return $ IntValue i
    
runConstructor :: Constructor -> TranslatorSt Value
runConstructor (Constructor n args) = do
    print' $ "run constructor " ++ n
    mbc <- findConstructor n
    assert (isJust mbc) "Not in scope: constructor" n
    let c = fromJust mbc
    tas <- translateArgs args
    assert (length tas == constructorArity c) "wrong arity:" (length tas)
    
    
    return $ StringValue ""

translateExpression :: Expr -> TranslatorSt Value
translateExpression (ConstructorExpr c) = runConstructor c
translateExpression (ConstantExpr c)    = runConstant c
translateExpression _ = error "translateExpression"

runStatement :: Statement -> TranslatorSt ()
runStatement c@(ConstantStmt name expr) = do
    assertNotExistIn constants name
    error "runStatement c@(ConstantStmt expr)"
runStatement c@(CallStmt expr) = do
    error "runStatement c@(CallStmt expr)"
    
runStatement c@(ValStmt name expr) = do
    r <- runValueStatement name expr
    
    error $ "runStatement c@(ValStmt expr)" ++ show r

runProcedureDef (ProcDef (ProcDecl n params) (ProcBody stmts)) = do
    print' $ "run procedure def: " ++ n
    setIndentation 1
    print' $ "indentation: 1"
    runLIStatements stmts

translateEntry :: ProgramEntry -> TranslatorSt ()
translateEntry LinedEmptyEntry = return ()
translateEntry (LinedEntry st) = error "translateEntry (LinedEntry st)"
translateEntry (ScriptEntry st pd) = do
    print' $ "translate script entry: " ++ show st
    enableScriptTranslation st
    runProcedureDef pd
    disableScriptTranslation
translateEntry _ = error "translateEntry"

translateProgram :: Program -> TranslatorSt ()
translateProgram (Program [])      = return ()
translateProgram (Program entries) = do
    print' $ "translate program"
    mapM_ translateEntry entries

fromAst :: Program -> TranslatorSt ()
fromAst = translateProgram

dataRelativeDir = "/../../../../../../test/Data/"
dataFile bd f = bd ++ dataRelativeDir ++ f

parseFromFile' f = do
    bd <- getBinDir  
    res <- parseFromFile program (dataFile bd f)
    case res of
         Left e -> error $ show e
         Right r -> return r
         
emptyTables = Tables ("constant", M.empty) ("value", M.empty) fillScriptsTable fillSysConstructorsTable
emptySt = Translator emptyTables (return ()) Nothing 0

test :: IO ()
test = do
    print "Translation test."

    res <- parseFromFile' "controller_script_simple1.txt"
    print res >> print ""
    (_, (Translator tables prog _ _)) <- S.runStateT (fromAst res) emptySt
    
    interpretControlProgram prog
    