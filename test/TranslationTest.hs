{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module TranslationTest where

import Andromeda
import TestCommon
import Paths_Andromeda

import Control.Lens hiding (getConst)
import Control.Monad.Trans.State as S
import Control.Monad
import qualified Data.Map as M
import Data.Maybe

type Table = (String, M.Map String String)

data Tables = Tables {
      _constants :: Table
    , _values :: Table
    , _scripts :: ScriptsTable
}

data Translator = Translator {
      _tables :: Tables
    , _controlProg :: ControlProgram ()
    , _scriptTranslation :: Maybe ScriptType
    , _indentation :: Int
}

makeLenses ''Tables
makeLenses ''Translator

type TranslatorSt a = S.State Translator a

enableScriptTranslation st = do
    checkNoScriptTranslation
    assign scriptTranslation (Just st)
disableScriptTranslation = assign scriptTranslation Nothing

setIndentation n = assign indentation n
incIndentation, decIndentation :: TranslatorSt ()
incIndentation = indentation += 1
decIndentation = do
    checkIndentation (>0)
    indentation -= 1
checkIndentation p = do
    i <- use indentation
    when (not $ p i) $ error $ "Wrong indentation: " ++ show i

getConst :: IdName -> TranslatorSt (Maybe String)
getConst n = use (tables . constants . _2 . at n)

checkNoScriptTranslation = do
    x <- use scriptTranslation
    when (isJust x) $ error $ "Script translation: " ++ show x

checkNotExistIn :: Lens' Tables Table  -> IdName -> TranslatorSt ()
checkNotExistIn table key = do
    n <- use $ tables . table . _1
    x <- use $ tables . table . _2 . at key
    when (isJust x) $ error $ n ++ " exist: " ++ key
    
translateExpr = error "translateExpr"


translateLIStatements [] = return ()
translateLIStatements (LinedEmptyStmt:stmts) = translateLIStatements stmts
translateLIStatements (LinedIndentedStmt (IndentedStmt i stmt):stmts) = do
    checkIndentation (==i)
    translateStatement stmt

translateProcedureDef (ProcDef (ProcDecl n params) (ProcBody stmts)) = do
    setIndentation 1
    translateLIStatements stmts


{-
data Statement = ConstantStmt IdName Expr
               | ValStmt IdName Expr
               | CallStmt Expr
               
data Expr = ConstantExpr Constant
          | ConstructorExpr Constructor
          | IdentifierExpr Identifier
  deriving (Show)
-}

findScriptConstructor :: ScriptType -> IdName -> TranslatorSt (Maybe Constr)
findScriptConstructor st n = do
    mbs <- use (tables . scripts . at st)
    case mbs of
         Nothing -> return Nothing
         Just t -> return $ view (at n) t

findProcedure = error "findProcedure"

findConstructor :: IdName -> TranslatorSt (Maybe Constr)
findConstructor n = do
    msc <- use scriptTranslation
    mbc <- case msc of
         Just s -> findScriptConstructor s n
         Nothing -> return Nothing
    case mbc of
         Nothing -> findProcedure n
         _ -> return mbc

runConstructor :: Constructor -> TranslatorSt ()
runConstructor (Constructor n args) = do
    c <- findConstructor n
    
    return ()

translateExpression :: Expr -> TranslatorSt ()
translateExpression (ConstructorExpr c) = do
    runConstructor c
translateExpression _ = error "translateExpression"

translateStatement :: Statement -> TranslatorSt ()
translateStatement c@(ConstantStmt name expr) = do
    checkNotExistIn constants name
    error "translateStatement c@(ConstantStmt expr)"
translateStatement c@(CallStmt expr) = do
    error "translateStatement c@(CallStmt expr)"
    
translateStatement c@(ValStmt name expr) = do
    checkNotExistIn values name
    translateExpression expr
    
    error "translateStatement c@(ValStmt expr)"
    
    
translateEntry :: ProgramEntry -> TranslatorSt ()
translateEntry LinedEmptyEntry = return ()
translateEntry (LinedEntry st) = translateStatement st
translateEntry (ScriptEntry st pd) = do
    enableScriptTranslation st
    translateProcedureDef pd
    disableScriptTranslation
translateEntry _ = error "translateEntry"

translateProgram :: Program -> TranslatorSt ()
translateProgram (Program [])      = return ()
translateProgram (Program entries) = mapM_ translateEntry entries

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
         
emptyTables = Tables ("constant", M.empty) ("value", M.empty) fillScriptsTable
emptySt = Translator emptyTables (return ()) Nothing 0

test :: IO ()
test = do
    print "Translation test."

    res <- parseFromFile' "controller_script_simple1.txt"
    print res >> print ""
    let (_, (Translator tables prog _ _)) = S.runState (fromAst res) emptySt
    
    interpretControlProgram prog
    