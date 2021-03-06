module Andromeda.Types.Common.Value where

data Value = BoolValue Bool
           | IntValue Int
           | FloatValue Float
           | StringValue String
  deriving (Show, Read, Eq, Ord)
  
class ToValue a where
  toValue :: a -> Value

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
