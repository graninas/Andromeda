module Andromeda.Language.External.LogicControl.AST where

data Constant = StringConstant String
              | IntegerConstant Int
  deriving (Show)

data ArgDef = NoneArgs
            | Args [Entry]
  deriving (Show)
  
data Constructor = Constructor String ArgDef
  deriving (Show)
  
data Entry = ConstantEntry Constant
           | ConstructorEntry Constructor
  deriving (Show)

data Declaration = ConstDecl String Entry
                 | BeginDecl String
                 | EndDecl String
  deriving (Show)
  
  
