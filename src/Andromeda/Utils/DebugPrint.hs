module Andromeda.Utils.DebugPrint where

import Control.Monad.IO.Class

debugPrint :: (Show v, MonadIO m) => v -> m ()
debugPrint = liftIO . print
