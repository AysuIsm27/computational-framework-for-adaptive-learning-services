{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module GiveFeedback where

-- | -----------------------------------------------------------------------
-- | Give Feedback Service
-- | -----------------------------------------------------------------------
-- | Definition: gives feedback or hints on a student product, potentially
-- | based on a learner model and/or a peer-learner model. A student product
-- | might be an essay, a program, a step in a calculation, an answer to a
-- | question, or something else.
-- |
-- | 'p' represents the student product (e.g., an essay or code snippet),
-- | 'm' is the model, and 'f' is the resulting feedback.
-- | -----------------------------------------------------------------------

-- | Evaluates a student product against a model to produce feedback.
class EvaluateProduct p m f | p m -> f where
  evaluate_product :: p -> m -> f

-- | The generic feedback function processes a product through a model.
give_feedback :: EvaluateProduct p m f => p -> m -> f
give_feedback product model = evaluate_product product model
