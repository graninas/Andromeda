module Andromeda.Language.External.LogicControl.Parser where

import Andromeda.Language.External.LogicControl.AST
import Andromeda.Common.Parsing

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec

eol' = string "\n"

stringConstantExpr  = fmap StringConstant stringConstant
integerConstantExpr = fmap IntegerConstant integerConstant
constantExpr        = fmap ConstantExpr (stringConstantExpr <|> integerConstantExpr) -- <|> floatConstantExpr


constructorName = do
    bigC <- upper
    smallCs <- many alphaNum
    return $ bigC : smallCs

argsList = do
    args <- listOf' '(' ')' expr
    return $ Args args
    
emptyArgDef = do
    between (char '(') (char ')') trueSpaces
    return NoneArgs
    
argDef = argsList <|> return NoneArgs
    
constructor = do
    n <- constructorName
    trueSpaces
    ad <- argDef
    return $ Constructor n ad

constructorExpr = do
    c <- constructor
    return $ ConstructorExpr c

identifier' = do
    idN <- identifier
    trueSpaces
    ad <- argDef
    return $ Identifier idN ad
    
identifierExpr = do
    identif <- identifier'
    return $ IdentifierExpr identif

expr = constantExpr <|> constructorExpr <|> identifierExpr
    
constantStatement = do
    string "const"
    trueSpaces
    constId <- identifier
    assignment
    e <- expr
    return $ ConstantStmt constId e

valStatement = do
    string "val"
    trueSpaces
    valId <- identifier
    assignment
    e <- expr
    return $ ValStmt valId e
    
callStatement = do
    c <- expr
    return $ CallStmt c

statement = constantStatement <|> valStatement <|> callStatement -- <|> ifThenElseStmt

indentation = count 4 (char ' ')

indentedStatement = do
    is <- many1 indentation -- mandatory indentation!!
    stmt <- statement
    return (IndentedStmt (length is) stmt)
       
linedIndentedStatement = do
    stmt <- indentedStatement
    return $ LinedIndentedStmt stmt
    
linedEmptyStatement = do
    many indentation
    return LinedEmptyStmt
    
linedStatement = do
    eol'
    linedIndentedStatement <|> linedEmptyStatement

param = do
    string "val"
    trueSpaces
    identifier
   
paramList = do
    ps <- listOf' '(' ')' param
    return $ Params ps

paramDef = paramList <|> return NoneParams

procedureStmt = do
    n <- constructorName
    trueSpaces
    pd <- paramDef
    trueSpaces
    char ':'
    return $ ProcDecl n pd    

procedureBody = do
    stmts <- many linedStatement
    return $ ProcBody stmts
    
procedure = do
    ps <- procedureStmt
    b <- procedureBody
    return $ Proc ps b
    
procedureEntry = do
    p <- procedure
    trueSpaces
    return $ ProcedureEntry p
    
linedEntry = do
    s <- statement
    trueSpaces
    return $ LinedEntry s
    
linedEmptyEntry = do
    eol'
    many indentation
    return LinedEmptyEntry

programEntry = procedureEntry <|> linedEntry <|> linedEmptyEntry

program = do
    es <- many programEntry
    return $ Program es
