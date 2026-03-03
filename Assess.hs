{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Assess where

-- | -----------------------------------------------------------------------
-- | Assess Service
-- | -----------------------------------------------------------------------
-- | Definition: estimates or validates a learner's knowledge, misconceptions,
-- | or mastery by presenting assessment items selected by the system and
-- | interpreting the learner's responses as evidence about their learning state.
-- |
-- | Because this requires two distinct actions (selecting an item, then
-- | interpreting the response), we use two type classes to form a pipeline
-- | similar to recommend.
-- | -----------------------------------------------------------------------

-- | The system selects the most informative assessment item for this learner.
--   The learner has no say in which item is chosen.
class SelectItem i m item | i m -> item where
  select_item :: i -> m -> item

-- | The system interprets the learner's response as evidence about their
--   knowledge state, producing a mastery estimate or diagnostic result.
class InterpretResponse i m item response mastery | i m item response -> mastery where
  interpret :: i -> m -> item -> response -> mastery

-- | The generic assess function selects an item, obtains the learner's
--   response via a function that takes the item and yields a response,
--   and interprets that response to produce a mastery result.
assess :: (SelectItem i m item, InterpretResponse i m item response mastery)
       => i -> m -> (item -> response) -> mastery
assess input model get_response =
  let presented_item   = select_item input model      -- Step 1: system selects item
      learner_response = get_response presented_item  -- Step 2: learner responds
  in  interpret input model presented_item learner_response  -- Step 3: system interprets
