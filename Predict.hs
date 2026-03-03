{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Predict where

-- | -----------------------------------------------------------------------
-- | Predict Service
-- | -----------------------------------------------------------------------
-- | Definition: predicts an observable result based on learner behaviour,
-- | a learner profile, a learner model, and/or a peer-learner model.
-- | An observable result can be a behaviour, performance, outcome, and more.
-- | -----------------------------------------------------------------------

-- | Maps a learner input and model to a predicted observable outcome.
class PredictObservable i m o | i m -> o where
  predict_result :: i -> m -> o

-- | The generic predict function maps an input and model to a predicted outcome.
predict_service :: PredictObservable i m o => i -> m -> o
predict_service input model = predict_result input model
