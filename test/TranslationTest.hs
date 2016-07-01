{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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


enableScriptTranslation st = do
    assertNoScriptTranslation
    assign scriptTranslation (Just st)
disableScriptTranslation = assign scriptTranslation Nothing
isScriptTranslation :: TranslatorSt Bool
isScriptTranslation = do
    mbst <- use scriptTranslation
    return $ isJust mbst
gesScriptTranslation = do
    b <- isScriptTranslation
    assert b "Not script translation" ""
    mbst <- use scriptTranslation
    return $ fromJust mbst
    
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

addScript :: IdName -> Script () -> TranslatorSt ()
addScript n scr = do
    mbscr <- use $ tables . scripts . at n
    assert (isNothing mbscr) "script already exist:" n
    (tables . scripts . at n) %= (\_ -> Just scr)

findScriptConstructor :: IdName -> ScriptType -> TranslatorSt (Maybe Constr)
findScriptConstructor n st = do
    mbs <- use (tables . scriptDefs . at st)
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

runArgs :: ArgDef -> TranslatorSt Created
runArgs NoneArgs  = return $ CreatedArgs []
runArgs (Args es) = do
    print' $ "runArgs"
    incPrintIndentation
    r <- mapM runExpression es
    decPrintIndentation
    return $ CreatedArgs r

runValueStatement name expr = do
    print' $ "runValueStatement " ++ name
    incPrintIndentation
    assertNotExistIn values name
    cr <- runScriptExpression expr
    decPrintIndentation
    return cr

runScriptExpression :: Expr -> TranslatorSt Created
runScriptExpression (ConstructorExpr c) = do
    print' $ "runScriptExpression ConstructorExpr"
    runConstructor c
runScriptExpression (ConstantExpr c) = do
    print' $ "runScriptExpression ConstantExpr"
    runConstant c
runScriptExpression _ = error "runScriptExpression"

runConstant :: Constant -> TranslatorSt Created
runConstant c = do
    print' $ "runConstant " ++ show c
    return $ CreatedConst c

runConstructor :: Constructor -> TranslatorSt Created
runConstructor (Constructor n args) = do
    print' $ "runConstructor " ++ n
    incPrintIndentation
    
    st <- gesScriptTranslation
    mbc <- findConstructor n
    assert (isJust mbc) "Not in scope: constructor" n
    let c = fromJust mbc
    crtas@(CreatedArgs tas) <- runArgs args
    assert (length tas == constructorArity c) "wrong arity:" (length tas)
    created <- createConstructor st (fromJust mbc) crtas
    case created of
         CreatedControllerScript scr -> do
             print' $ "--> controller script created: "
             res <- liftIO $ interpretControllerScript scr
             print' $ "--> script interpreter result: " ++ show res
         CreatedConstr c -> do
             print' $ "--> constructor created: " ++ show c
             
    decPrintIndentation
    return $ created

runExpression :: Expr -> TranslatorSt Created
runExpression (ConstructorExpr c) = do
    print' $ "runExpression ConstructorExpr"
    runConstructor c
runExpression (ConstantExpr c) = do
    print' $ "runExpression ConstantExpr"
    runConstant c
runExpression _ = error "runExpression"

runStatement :: Statement -> TranslatorSt Composed
runStatement c@(ConstantStmt name expr) = do
    print' $ "runStatement ConstantStmt " ++ name
    assertNotExistIn constants name
    error "runStatement c@(ConstantStmt expr)"
runStatement c@(CallStmt expr) = do
    error "runStatement c@(CallStmt expr)"
runStatement c@(ValStmt name expr) = do
    print' $ "runStatement ValStmt " ++ name
    created <- runValueStatement name expr
    return $ ComposedVal name created

runLIStatements [] = return []
runLIStatements (LinedEmptyStmt:stmts) = runLIStatements stmts
runLIStatements (LinedIndentedStmt (IndentedStmt i stmt):stmts) = do
    assertIndentation (==i)
    composed <- runStatement stmt
    composeds <- runLIStatements stmts
    return $ composed : composeds

{-
runControllerScriptResolving :: [Composed] -> TranslatorSt ScriptResolved
runControllerScriptResolving [] = return NoneScriptResolved
runControllerScriptResolving (ComposedVal n (CreatedControllerScript scr):rs) = do
    next <- runControllerScriptResolving rs
    case next of
         NoneScriptResolved -> return $ ContrScriptResolved scr
         ContrScriptResolved composedScr -> let scr' = scr >> composedScr
                                            in return $ ContrScriptResolved scr'
                                            -}
                                            
runControllerScriptResolving :: [Composed] -> ControllerScript ()
runControllerScriptResolving [] = return ()
runControllerScriptResolving (ComposedVal n (CreatedControllerScript scr):rs) = do
    scr
    runControllerScriptResolving rs
    
runResolving composeds = do
    st <- gesScriptTranslation
    case st of
         ControllerScriptDef -> do
             let scr = runControllerScriptResolving composeds
             return $ ContrScriptResolved scr
         _ -> error "runResolving"
    
runProcedureDef (ProcDef (ProcDecl n params) (ProcBody stmts)) = do
    print' $ "runProcedureDef: " ++ n
    setIndentation 1
    print' $ "indentation: 1"
    composeds <- runLIStatements stmts
    resolved <- runResolving composeds
    case resolved of
         ContrScriptResolved scr -> do
             print' $ "controller script resolved."
             liftIO $ interpretControllerScript scr
             addScript n (controllerScript scr)

translateEntry :: ProgramEntry -> TranslatorSt ()
translateEntry LinedEmptyEntry = return ()
translateEntry (LinedEntry st) = error "translateEntry LinedEntry"
translateEntry (ScriptEntry st pd) = do
    print' $ "translateEntry ScriptEntry" ++ show st
    incPrintIndentation
    enableScriptTranslation st
    runProcedureDef pd
    disableScriptTranslation
    decPrintIndentation
translateEntry _ = error "translateEntry"

translateProgram :: Program -> TranslatorSt ()
translateProgram (Program [])      = return ()
translateProgram (Program entries) = do
    print' $ "translateProgram"
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
         
emptyTables = Tables ("constant", M.empty) ("value", M.empty) fillScriptsDefsTable fillSysConstructorsTable M.empty
emptySt = Translator emptyTables (return ()) Nothing 0 0

    
test :: IO ()
test = do
    print "Translation test."
    
    res <- parseFromFile' "controller_script_simple1.txt"
    print res >> print ""
    (_, (Translator tables prog _ _ _)) <- S.runStateT (fromAst res) emptySt
    
    interpretControlProgram prog
    