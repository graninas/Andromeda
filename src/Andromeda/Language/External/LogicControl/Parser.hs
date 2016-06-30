module Andromeda.Language.External.LogicControl.Parser where

import Andromeda.Language.External.LogicControl.AST
import Andromeda.Common.Parsing

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec

stringConstantExpr  = fmap StringConstant stringConstant
integerConstantExpr = fmap IntegerConstant integerConstant
constantExpr        = fmap ConstantExpr (stringConstantExpr <|> integerConstantExpr) -- <|> floatConstantExpr
identifierExpr      = fmap IdentifierExpr identifier

constructorName = do
    bigC <- upper
    smallCs <- many lower
    return $ bigC : smallCs

argDef = do
    args <- listOf' '(' ')' expr
    return $ Args args

constructor = do
    trueSpaces
    n <- constructorName
    trueSpaces
    ad <- argDef <|> return NoneArgs
    return $ Constructor n ad

constructorExpr = do
    c <- constructor
    return $ ConstructorExpr c
    
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
    ce <- constructorExpr
    return $ CallStmt ce

statement = constantStatement <|> valStatement <|> callStatement -- <|> ifThenElseStmt

indentation = count 4 (char ' ')

indentedStatement = do
    is <- many1 indentation -- mandatory indentation!!
    stmt <- statement
    return (IndentedStmt (length is) stmt)

procedureDeclaration = do
    n <- constructorName
    trueSpaces
    ad <- argDef
    trueSpaces
    char ':'
    trueSpaces
    stmts <- many indentedStatement
    return $ ProcDecl n ad stmts
    
{-
    
constrDeclaration = do
    string "begin"
    trueSpaces
    bId <- identifier
    return $ BeginDecl bId

endDeclaration = do
    string "end"
    trueSpaces
    eId <- identifier
    return $ EndDecl eId
    
data Expr = ConstantExpr Constant
          | ConstructorExpr Constructor
          | IdentifierExpr IdName
  deriving (Show)

data Statement = ConstantStmt IdName Expr
               | ValStmt IdName Expr
  deriving (Show)

data IndentedDeclaration = IndentedDecl Int Statement
  deriving (Show)
  
data ProcedureDeclaration = ProcDecl IdName [ArgDef] [IndentedDeclaration]
  deriving (Show)
  -}