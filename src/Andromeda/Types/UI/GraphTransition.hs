{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Andromeda.Types.UI.GraphTransition where

import Control.Monad.State (execState, runState, evalState, get, put, State(..))
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Free (Free(..), liftF, foldFree)
import Control.Monad (when, void)

import Andromeda.Common.Exists

data LangF a = PrintS String a
type Lang a = Free LangF a

instance Functor LangF where
  fmap f (PrintS s next) = PrintS s (f next)

printS :: String -> Lang ()
printS s = liftF $ PrintS s ()

type Event = String

data TransitionF b o u
  = Backable Event (Graph b o) u
  | ForwardOnly Event (Graph b o) u

type Transition b o u = Free (TransitionF b o) u

data GraphF i o b
  = GraphF (Lang b) (Transition b o ())
  | GraphF1 (i -> Lang b) (Transition b o ())

data Graph i o = Graph (Exists (GraphF i o))

type PartialTrans i o b = Transition b o () -> Graph i o
data Event' i o = Event' Event (Graph i o)

instance Functor (TransitionF b o) where
  fmap f (Backable e g next) = Backable e g (f next)
  fmap f (ForwardOnly e g next) = ForwardOnly e g (f next)

(<~>) = backable''
(~>) = forwardOnly''

infixl 3 <~>
infixl 3 ~>

with :: forall b o. Lang b -> Transition b o () -> Graph () o
with flow table = Graph $ mkExists $ GraphF flow table

with1 :: forall i b o. (i -> Lang b) -> Transition b o () -> Graph i o
with1 flowF1 table = Graph $ mkExists $ GraphF1 flowF1 table

leaf :: Lang () -> Graph () ()
leaf flow = with flow (pure ())

leaf1 :: forall i. (i -> Lang ()) -> Graph i ()
leaf1 flowF1 = with1 flowF1 (pure ())

graph part = part $ pure ()

on = Event'

transable transType part (Event' e g) = part . transed
  where
    transed prevTrans = do
      prevTrans
      transType e g

backable' :: Event -> Graph i o -> Transition i o ()
backable' e g = liftF $ Backable e g ()

forwardOnly' :: Event -> Graph i o -> Transition i o ()
forwardOnly' e g = liftF $ ForwardOnly e g ()

backable'' = transable backable'
forwardOnly'' = transable forwardOnly'

---- Evaluation

data TrackResult a = BackTrack a | ForwardTrack a | Nop
data LangResult a b = Forward a b | Backward
data TransitionResult = Fallback | FallbackRerun | Done

interpretTransition :: forall b o u.
  Event -> TransitionF b o u -> State (TrackResult (Graph b o)) u
interpretTransition e (Backable expectedE g next) = do
  when (e == expectedE) (put $ BackTrack g)
  pure next
interpretTransition e (ForwardOnly expectedE g next) = do
  when (e == expectedE) (put $ ForwardTrack g)
  pure next

runTransition' :: forall b o s.
  Event -> Transition b o s -> State (TrackResult (Graph b o)) s
runTransition' e = foldFree (interpretTransition e)

runTransition :: forall i o b. Event -> GraphF i o b -> TrackResult (Graph b o)
runTransition e (GraphF _ t) = execState (runTransition' e t) Nop
runTransition e (GraphF1 _ t) = execState (runTransition' e t) Nop

runLang' :: forall m a. Monad m =>
  GraphRuntime m -> Lang a -> m (LangResult Event a)
runLang' (GraphRuntime runLang isBackEvent) flow = do
  (e, i) <- runLang flow
  if isBackEvent e
    then pure Backward
    else pure $ Forward e i

getLang :: forall i o b. i -> GraphF i o b -> Lang b
getLang _     (GraphF  flow  _) = flow
getLang input (GraphF1 flowF _) = flowF input

makeTransition' :: forall m i o b. Monad m =>
  GraphRuntime m -> Bool -> i -> GraphF i o b -> m TransitionResult
makeTransition' runtime backable i3 g3 = do
  let f3 = getLang i3 g3
  transitionResult <- makeTransition runtime f3 g3
  case transitionResult of
    Fallback -> if backable
      then pure FallbackRerun
      else pure Done -- throw "No fallback"
    Done -> pure Done
    FallbackRerun -> makeTransition' runtime backable i3 g3

makeTransition :: forall m i o b. Monad m =>
  GraphRuntime m -> Lang b -> GraphF i o b -> m TransitionResult
makeTransition runtime f2 g2 = do
  flowResult <- runLang' runtime f2
  case flowResult of
    Forward e2 i3 -> do
      let trackResult = runTransition e2 g2
      case trackResult of
        Nop -> pure Done
        BackTrack g3@(Graph g3Ex) -> runExists (makeTransition' runtime True i3) g3Ex
        ForwardTrack g3@(Graph g3Ex) -> runExists (makeTransition' runtime False i3) g3Ex
    Backward -> pure Fallback

data GraphRuntime m = GraphRuntime
  { runLang_ :: forall output. Lang output -> m (Event, output)
  , isBackEvent_ :: Event -> Bool
  }

runGraph :: forall m. Monad m => GraphRuntime m -> Graph () () -> m ()
runGraph runtime (Graph ex) = do
  _ <- runExists (makeTransition' runtime False ()) ex
  pure ()
