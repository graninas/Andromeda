module Andromeda.Language.External.LogicControl.AST where

type IdName = String
type ConstrName = String

data Constant = StringConstant String
              | IntegerConstant Int
  deriving (Show)

data ArgDef = NoneArgs
            | Args [Expr]
  deriving (Show)
  
data Constructor = Constructor ConstrName ArgDef
  deriving (Show)
 
data Expr = ConstantExpr Constant
          | ConstructorExpr Constructor
          | IdentifierExpr IdName
  deriving (Show)
 
data Statement = ConstantStmt IdName Expr
               | ValStmt IdName Expr
               | CallStmt Expr
  deriving (Show)

data IndentedStatement = IndentedStmt Int Statement
  deriving (Show)
  
data ProcedureDeclaration = ProcDecl IdName ArgDef [IndentedStatement]
  deriving (Show)