{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Simplified example based on:
--
-- Maniktala, M., Cody, C., Isvik, A., Lytle, N., Chi, M., & Barnes, T. (2020).
-- "Extending the Hint Factory for the Assistance Dilemma:
-- A Novel, Data-driven HelpNeed Predictor for Proactive Problem-solving Help."
-- Journal of Educational Data Mining, 12(4), 24--65.
-- DOI: 10.5281/zenodo.4399683
--
-- This implementation is intentionally simplified. It uses state quality
-- and Global-Absolute progress as an example signal for deciding when a
-- proactive hint may be useful; it does not implement the complete
-- HelpNeed predictor from the paper.

module HintFactory where

import Data.Map (Map)
import qualified Data.Map as Map

import Hint (GenerateHint (..), hint, hint_batch)


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
  } deriving (Eq, Show)


data Advice = Advice
  { adviceStep      :: Step
  , adviceNextState :: State
  } deriving (Eq, Show)


-- Generate a hint for a known task state.
instance GenerateHint State Model (Maybe Advice) where
  generate_hint state model = do
    (step, nextState) <- Map.lookup state (bestNext model)
    return (Advice step nextState)


-- A learner consists of the model and the state
-- where the learner started the problem.
type Learner = (Model, State)


instance GenerateHint State Learner (Maybe Advice) where
  generate_hint state (model, _) =
    generate_hint state model


-- Simplified Global-Absolute efficiency.
-- True means the learner is currently in a lower-quality
-- state than at the beginning of the problem.
needsHelp :: State -> Learner -> Maybe Bool
needsHelp current (model, startState) = do
  currentQuality <- Map.lookup current (quality model)
  startQuality   <- Map.lookup startState (quality model)

  return (currentQuality < startQuality)


-- Generate a proactive hint when help is needed.
proactiveHint :: State -> Learner -> Maybe Advice
proactiveHint state learner =
  case needsHelp state learner of
    Just True -> hint state learner
    _         -> Nothing


-- Generate one hint using the generic Hint service.
getSuggestion :: State -> Model -> Maybe Advice
getSuggestion = hint


-- Generate hints for several task states.
trajectoryHints :: [State] -> Model -> [(State, Maybe Advice)]
trajectoryHints = hint_batch
