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
    
translateProgram :: Program -> TranslatorSt ()
translateProgram (Program [])      = return ()
translateProgram (Program entries) = mapM_ translateEntry entries

{-
data Statement = ConstantStmt IdName Expr
               | ValStmt IdName Expr
               | CallStmt Expr
               
data Expr = ConstantExpr Constant
          | ConstructorExpr Constructor
          | IdentifierExpr Identifier
  deriving (Show)
-}

translateExpr = error "translateExpr"

translateIStatement :: Statement -> TranslatorSt ()
translateIStatement (CallStmt e) = do
    translateExpr e
translateIStatement (ValStmt n e) = do
    checkNotExistIn values n

translateLIStatements [] = return ()
translateLIStatements (LinedEmptyStmt:stmts) = translateLIStatements stmts
translateLIStatements (LinedIndentedStmt (IndentedStmt i stmt):stmts) = do
    checkIndentation (==i)
    translateStatement stmt

translateProcedureDef (ProcDef (ProcDecl n params) (ProcBody stmts)) = do
    setIndentation 1
    translateLIStatements stmts

translateEntry :: ProgramEntry -> TranslatorSt ()
translateEntry (ScriptEntry st pd) = do
    enableScriptTranslation st
    translateProcedureDef pd
    disableScriptTranslation
    
    
translateEntry LinedEmptyEntry = return ()
translateEntry (LinedEntry st) = translateStatement st

translateStatement :: Statement -> TranslatorSt ()
translateStatement c@(ConstantStmt name expr) = do
    checkNotExistIn constants name
    -- TODO
    


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
         
emptyTables = Tables ("constant", M.empty) ("value", M.empty)
emptySt = Translator emptyTables (return ()) Nothing 0

test :: IO ()
test = do
    print "Translation test."

    res <- parseFromFile' "controller_script_simple1.txt"
    let (_, (Translator tables prog _ _)) = S.runState (fromAst res) emptySt
    
    interpretControlProgram prog
    