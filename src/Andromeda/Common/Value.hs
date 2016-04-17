module Andromeda.Common.Value
(
    trueValue,
    falseValue,
    boolValue,
    stringValue,
    intValue,
    floatValue,
    Value(..)
) where

data Value = BoolValue Bool
           | IntValue Int
           | FloatValue Float
           | StringValue String           
  deriving (Show, Read, Eq)


trueValue = BoolValue True
falseValue = BoolValue False

boolValue :: Bool -> Value
boolValue = BoolValue

stringValue :: String -> Value
stringValue = StringValue

intValue :: Int -> Value
intValue = IntValue

floatValue :: Float -> Value
floatValue = FloatValue
