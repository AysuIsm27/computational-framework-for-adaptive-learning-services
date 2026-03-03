{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Recommend where

-- | -----------------------------------------------------------------------
-- | Recommend Service
-- | -----------------------------------------------------------------------
-- | Definition: recommends a learning activity based on learner behaviour,
-- | a learner profile, a learner model, and/or a peer-learner model.
-- | The receiver (student, teacher, or researcher) retains the choice to
-- | follow the recommendation or not. The output is a ranked list of
-- | alternatives (task, material, course, learning path, curriculum…).
-- |
-- | Because recommendation requires two distinct steps (finding candidates,
-- | then ranking them), we use two type classes to form a pipeline.
-- | -----------------------------------------------------------------------

-- | Determines the set of candidate items eligible for this learner.
class Candidates i m c | i m -> c where
  candidates :: i -> m -> [c]

-- | Selects the best item (or ordered list) from the set of candidates.
--   'i' is included so that best has access to the original learner context.
class Best i m c | i m -> c where
  best :: i -> m -> [c] -> [c]

-- | The generic recommend function pipelines candidate selection and ranking,
--   returning a ranked list from which the receiver may choose.
recommend :: (Candidates i m c, Best i m c) => i -> m -> [c]
recommend input model = best input model (candidates input model)

-- | Convenience: return only the top-k recommendations.
recommendTopK :: (Candidates i m c, Best i m c) => Int -> i -> m -> [c]
recommendTopK k input model = take k (recommend input model)
