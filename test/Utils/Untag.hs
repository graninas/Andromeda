module Test.Utils.Untag where

import Unsafe.Coerce

-- See Andromeda.LogicControl.Language.
-- Don't know how to do this rihgt in Haskell...
untag = unsafeCoerce
