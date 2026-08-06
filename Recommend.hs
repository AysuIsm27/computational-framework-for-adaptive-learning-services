{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Recommend
  ( Model (..)
  , Candidates (..)
  , Ranking (..)
  , recommend
  , recommendTopN
  ) where

-- | Represents a learner model or peer-learner model.
-- |
-- | The functional dependency states that the model type determines
-- | the type of evidence used to update it.
class Model m l | m -> l where
  initModel :: m
  update    :: l -> m -> m

-- | Determines the candidate objects available for recommendation.
-- |
-- | Candidate generation may depend on both the service input and
-- | the learner or peer-learner model.
class Candidates i m c where
  candidates :: i -> m -> [c]

-- | Orders recommendation candidates from most to least suitable.
class Ranking i m c where
  rank :: i -> m -> [c] -> [c]

-- | Produces a ranked list of recommendations.
recommend
  :: ( Model m l
     , Candidates i m c
     , Ranking i m c
     )
  => i
  -> m
  -> [c]
recommend input model =
  rank input model (candidates input model)

-- | Returns at most the first N recommendations.
-- | Non-positive values of N return an empty list.
recommendTopN
  :: ( Model m l
     , Candidates i m c
     , Ranking i m c
     )
  => Int
  -> i
  -> m
  -> [c]
recommendTopN n input model =
  take (max 0 n) (recommend input model)
