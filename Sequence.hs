{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Sequence where

-- | -----------------------------------------------------------------------
-- | Sequence Service
-- | -----------------------------------------------------------------------
-- | Definition: the system determines a single learning activity to be
-- | performed next. Unlike recommend, there is no list of candidates for
-- | the user to choose from.
-- | -----------------------------------------------------------------------

-- | The system maps a learner input and model directly to one activity.
class DetermineNext i m a | i m -> a where
  determine_next :: i -> m -> a

-- | The generic sequence function maps an input and model to one activity.
sequence_activity :: DetermineNext i m a => i -> m -> a
sequence_activity input model = determine_next input model
