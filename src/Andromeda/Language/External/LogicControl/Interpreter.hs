module Andromeda.Language.External.LogicControl.Interpreter where

import Andromeda.Common
import Andromeda.LogicControl as LC hiding (read)
import qualified Andromeda.LogicControl as LC (read)
import Andromeda.Language.External.LogicControl.AST

import Control.Lens hiding (getConst)
import Control.Monad.Trans.State as S
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe



    
toAst :: ControlProgram () -> Program
toAst p = undefined

