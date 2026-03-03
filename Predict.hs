{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Predict where

-- | -----------------------------------------------------------------------
-- | Predict Service
-- | -----------------------------------------------------------------------
-- | Definition: predicts an observable result based on learner behaviour,
-- | a learner profile, a learner model, and/or a peer-learner model.
-- | An observable result can be a behaviour, performance, outcome, or any
-- | measurable quantity of interest (e.g. pass/fail, test score, dropout).
-- | -----------------------------------------------------------------------

-- | Maps a learner input and model to a predicted observable outcome.
class PredictObservable i m o | i m -> o where
  predict_result :: i -> m -> o

-- | The generic predict function maps a learner input and model to a
--   predicted observable outcome.
predict_service :: PredictObservable i m o => i -> m -> o
predict_service = predict_result

-- | Applies the prediction model to every learner in a cohort, returning
--   each learner paired with their predicted outcome.
--   Useful for batch early-warning processing across a whole class.
predict_cohort :: PredictObservable i m o => [i] -> m -> [(i, o)]
predict_cohort learners model = map (\i -> (i, predict_result i model)) learners

-- | Partitions a cohort into at-risk and not-at-risk groups.
--   The predicate decides whether a predicted outcome counts as at-risk
--   (e.g. \o -> passProbability o < 0.5).
flag_at_risk :: PredictObservable i m o
             => (o -> Bool)  -- ^ At-risk predicate on the predicted outcome
             -> [i] -> m -> ([i], [i])
flag_at_risk is_at_risk learners model =
  let scored = predict_cohort learners model
  in  ( map fst (filter (is_at_risk . snd) scored)
      , map fst (filter (not . is_at_risk . snd) scored) )
