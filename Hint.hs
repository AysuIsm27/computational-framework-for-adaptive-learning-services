{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hint where

-- | -----------------------------------------------------------------------
-- | Hint Service
-- | -----------------------------------------------------------------------
-- | Definition: provides guidance on how to progress in a learner's current
-- | task, based on a learner model and/or a peer-learner model.
-- |
-- | A hint may direct attention to relevant information or suggest a concept,
-- | strategy, intermediate goal, or productive next step without necessarily
-- | revealing the complete solution.
-- |
-- | 't' represents the learner's current task state,
-- | 'm' is the learner model, peer-learner model, or combination of models,
-- | and 'h' is the resulting hint.
-- | -----------------------------------------------------------------------

-- | Generates a forward-looking hint for the learner's current task state.
--
-- The functional dependency states that the combination of task-state type
-- and model type determines the resulting hint type.
class GenerateHint t m h | t m -> h where
  generate_hint :: t -> m -> h

-- | Maps a task state and learner model to a tailored hint.
hint :: GenerateHint t m h => t -> m -> h
hint = generate_hint

-- | Generates hints for a batch of task states using the same learner model.
-- Returns each task state paired with its hint, preserving input order.
hint_batch :: GenerateHint t m h => [t] -> m -> [(t, h)]
hint_batch tasks model =
  map generate tasks
  where
    generate task = (task, generate_hint task model)

-- | Provides a hint only when help is requested or insufficient progress is
-- detected.
hint_when
  :: GenerateHint t m h
  => (t -> m -> Bool) -- ^ Whether a hint is currently needed
  -> t                -- ^ Current task state
  -> m                -- ^ Learner model
  -> Maybe h
hint_when needs_hint task model
  | needs_hint task model = Just (generate_hint task model)
  | otherwise             = Nothing

-- | Iteratively generates and applies hints until:
--
-- * sufficient progress has been made;
-- * the maximum number of hint cycles has been reached; or
-- * the maximum number of cycles is zero or negative.
--
-- Returns:
--
-- * the final task state;
-- * the last generated hint, if any; and
-- * the number of completed hint cycles.
--
-- No hint is generated when the initial task state already satisfies the
-- progress condition or when the cycle limit is zero.
hint_until_progress
  :: GenerateHint t m h
  => (t -> Bool)        -- ^ Whether sufficient progress has been made
  -> (t -> h -> t)      -- ^ Apply a hint to produce a new task state
  -> Int                -- ^ Maximum number of hint cycles
  -> t                  -- ^ Initial task state
  -> m                  -- ^ Learner model
  -> (t, Maybe h, Int)
hint_until_progress progressed advance max_rounds task model =
  go task Nothing 0
  where
    round_limit = max 0 max_rounds

    go current_task last_hint completed_rounds
      | progressed current_task =
          (current_task, last_hint, completed_rounds)

      | completed_rounds >= round_limit =
          (current_task, last_hint, completed_rounds)

      | otherwise =
          let generated_hint = generate_hint current_task model
              next_task      = advance current_task generated_hint
          in go
               next_task
               (Just generated_hint)
               (completed_rounds + 1)          => (t -> m -> Bool) -- ^ Whether a hint is currently needed
          -> t
          -> m
          -> Maybe h
hint_when needs_hint task model
  | needs_hint task model = Just (generate_hint task model)
  | otherwise             = Nothing

-- | Iteratively generates and applies hints until sufficient progress is made,
--   or a maximum number of hint cycles is exhausted.
--   Returns the final task state, the last hint, and the number of cycles.
hint_until_progress :: GenerateHint t m h
                    => (t -> Bool)   -- ^ Whether sufficient progress was made
                    -> (t -> h -> t) -- ^ Update the task state after a hint
                    -> Int           -- ^ Maximum number of hint cycles
                    -> t
                    -> m
                    -> (t, h, Int)
hint_until_progress progressed advance max_rounds task model =
  go task 0
  where
    go t n =
      let h  = generate_hint t model
          t' = advance t h
      in if progressed t' || n + 1 >= max_rounds
           then (t', h, n + 1)
           else go t' (n + 1)
