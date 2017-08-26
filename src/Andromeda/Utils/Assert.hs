module Andromeda.Utils.Assert where

assert False msg ext = error $ msg ++ " " ++ show ext
assert _ _ _ = return ()
