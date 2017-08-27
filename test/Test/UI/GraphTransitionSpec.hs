{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.UI.GraphTransitionSpec where

import Prelude hiding (read)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Free (Free(..), liftF, foldFree)
import Test.Hspec

import Andromeda.Types.UI

printLevel :: String -> Lang ()
printLevel = printS

travel3Graph :: Graph () ()
travel3Graph = graph $
  with (printLevel "3")
    <~> on "forward" (leaf (return ()))

travel2Graph :: Graph () ()
travel2Graph = graph $
  with (printLevel "2")
    <~> on "forward" travel3Graph

travel1Graph :: Graph () ()
travel1Graph = graph $
  with (printLevel "1")
    <~> on "forward" travel2Graph

interpretLang :: LangF s -> IO s
interpretLang (PrintS s next) = print s >> return next

runLang :: Lang s -> IO (Event, s)
runLang l = do
  r <- foldFree interpretLang l
  return ("forward", r)

isBkEv "back" = True
isBkEv _ = False


spec = describe "Graph transitions test." $
  it "Test graph transitions." $ do
    runGraph (GraphRuntime runLang isBkEv) travel1Graph
    pending
