{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Scaffold where

-- | -----------------------------------------------------------------------
-- | Scaffold Service
-- | -----------------------------------------------------------------------
-- | Definition: supports a learner's engagement in a task that would be
-- | difficult to accomplish independently, based on a learner model and/or
-- | a peer-learner model.
-- |
-- | Scaffolding provides contingent support adapted to the learner's current
-- | level of understanding. It may structure or simplify the task, direct
-- | attention, model strategies, or guide learner actions.
-- |
-- | 't' represents the learner's current task state,
-- | 'm' is the learner model, peer-learner model, or combination of models,
-- | and 's' is the resulting scaffold.
-- | -----------------------------------------------------------------------

-- | Generates contingent support for the learner's current task state.
class GenerateScaffold t m s | t m -> s where
  generate_scaffold :: t -> m -> s

-- | Maps a task state and learner model to an adapted scaffold.
scaffold :: GenerateScaffold t m s => t -> m -> s
scaffold = generate_scaffold

-- | Generates scaffolds for a batch of task states using the same learner
--   model. Returns each task state paired with its scaffold, preserving order.
scaffold_batch :: GenerateScaffold t m s => [t] -> m -> [(t, s)]
scaffold_batch tasks model =
  map (\t -> (t, generate_scaffold t model)) tasks

-- | Provides a scaffold only when the learner requires additional support.
scaffold_when :: GenerateScaffold t m s
              => (t -> m -> Bool) -- ^ Whether scaffolding is currently needed
              -> t
              -> m
              -> Maybe s
scaffold_when needs_scaffold task model
  | needs_scaffold task model = Just (generate_scaffold task model)
  | otherwise                 = Nothing

-- | Iteratively applies scaffolding until the learner can proceed
--   independently, or a maximum number of scaffold cycles is exhausted.
--   Returns the final task state, the last scaffold, and the number of cycles.
scaffold_until_independent :: GenerateScaffold t m s
                           => (t -> Bool)   -- ^ Whether support is no longer needed
                           -> (t -> s -> t) -- ^ Update the task engagement
                           -> Int           -- ^ Maximum number of scaffold cycles
                           -> t
                           -> m
                           -> (t, s, Int)
scaffold_until_independent independent apply_scaffold max_rounds task model =
  go task 0
  where
    go t n =
      let s  = generate_scaffold t model
          t' = apply_scaffold t s
      in if independent t' || n + 1 >= max_rounds
           then (t', s, n + 1)
           else go t' (n + 1)
