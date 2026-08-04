{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- A simplified version of the Hint Factory / HelpNeed approach from
-- Maniktala, Cody, Isvik, Lytle, Chi & Barnes (2020), "Extending the
-- Hint Factory for the Assistance Dilemma", JEDM 12(4).
--
-- The paper, in terms of our GenerateHint class from Hint.hs:
--   t (task state) = a snapshot of a student's in-progress proof
--   m (model)       = historical student solutions, mined into "from
--                      this state, what's the best next step, and how
--                      good is this state" (their interaction network
--                      + state quality values -- here we use a plain
--                      distance-to-goal score as a stand-in for their
--                      Global Quality Value, not their actual
--                      Bellman-equation value iteration)
--   h (hint)        = a suggested next step (their "Get Suggestion"
--                      output / proactive hint)
--
-- Their second contribution, HelpNeed, is a classifier deciding
-- WHETHER a step needs help at all (Table 1/2: Expert-like, Strategic,
-- Opportunistic = no help needed; Far Off, Futile = help needed). We
-- use their best-performing definition, Global-Absolute efficiency
-- (section 4.1.2.5): compare the current state's quality to the
-- quality of the state the student started the problem in -- if it
-- hasn't improved, HelpNeed is true
module HintFactory where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

import Hint (GenerateHint (..), hint, hint_batch, hint_until_progress, hint_when)

type State = String
type Step  = String

data Move = Move
  { moveFrom :: State
  , moveStep :: Step
  , moveTo   :: State
  } deriving (Eq, Show)

data Model = Model
  { bestNext :: Map State (Step, State)
  , quality  :: Map State Int
  } deriving Show

buildModel :: [Move] -> [State] -> Model
buildModel moves goalStates = loop goalStates initialQuality Map.empty
  where
    predecessorsOf state =
      [ (moveFrom m, moveStep m) | m <- moves, moveTo m == state ]

    initialQuality = Map.fromList [ (g, 0) | g <- goalStates ]

    loop [] qual next = Model next qual
    loop (state : queue) qual next =
      let q      = qual Map.! state
          new    = [ (from, step) | (from, step) <- predecessorsOf state
                                   , not (Map.member from qual) ]
          qual'  = foldl' (\m (from, _)    -> Map.insert from (q - 1) m)       qual new
          next'  = foldl' (\m (from, step) -> Map.insert from (step, state) m) next new
          queue' = queue ++ map fst new
      in loop queue' qual' next'

data Advice = Advice
  { adviceStep      :: Step
  , adviceNextState :: State
  , adviceQuality   :: Int
  } deriving (Eq, Show)

instance GenerateHint State Model (Maybe Advice) where
  generate_hint state model = do
    (step, nextState) <- Map.lookup state (bestNext model)
    q                  <- Map.lookup nextState (quality model)
    return (Advice step nextState q)

type Learner = (Model, State)

instance GenerateHint State Learner (Maybe Advice) where
  generate_hint state (model, _) = generate_hint state model

helpNeeded :: State -> Learner -> Bool
helpNeeded current (model, startState) =
  currentQuality < startQuality
  where
    currentQuality = Map.findWithDefault minBound current    (quality model)
    startQuality   = Map.findWithDefault minBound startState (quality model)

getSuggestion :: State -> Model -> Maybe Advice
getSuggestion = hint

proactiveHint :: State -> Learner -> Maybe (Maybe Advice)
proactiveHint = hint_when helpNeeded

trajectoryHints :: [State] -> Model -> [(State, Maybe Advice)]
trajectoryHints = hint_batch

applyAdvice :: State -> Maybe Advice -> State
applyAdvice current Nothing       = current
applyAdvice _       (Just advice) = adviceNextState advice

walkToGoal
  :: [State] -> Int -> State -> Model
  -> (State, Maybe (Maybe Advice), Int)
walkToGoal goalStates = hint_until_progress isGoal applyAdvice
  where
    isGoal s = s `elem` goalStates

