module Andromeda.Language.External.LogicControl.Parser where

import Andromeda.Language.External.LogicControl.AST
import Andromeda.Common.Parsing

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec

stringConstant  = fmap StringConstant stringConst
integerConstant = fmap IntegerConstant integerConst
constantEntry   = fmap ConstantEntry (stringConstant <|> integerConstant) -- <|> floatConstant

argDef = do
    trueSpaces
    args <- listOf' '(' ')' entry
    return $ Args args

constructor = do
    trueSpaces
    bigC <- upper
    smallCs <- many lower
    trueSpaces
    ad <- argDef <|> return NoneArgs
    return $ Constructor (bigC : smallCs) ad

constructorEntry = do
    c <- constructor
    return $ ConstructorEntry c
    
entry = constantEntry <|> constructorEntry
    
constDeclaration = do
    string "const"
    trueSpaces
    constId <- identifier
    assignment
    e <- entry
    return $ ConstDecl constId e

beginDeclaration = do
    string "begin"
    trueSpaces
    bId <- identifier
    return $ BeginDecl bId

endDeclaration = do
    string "end"
    trueSpaces
    eId <- identifier
    return $ EndDecl eId

