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
-- | 'p' represents the student product (e.g., an essay, code, or answer),
-- | 'm' is the model used to evaluate it, and 'f' is the resulting feedback.
-- | -----------------------------------------------------------------------
-- | Evaluates a student product against a model and produces feedback.
class EvaluateProduct p m f | p m -> f where
  evaluate_product :: p -> m -> f
-- | The generic feedback function maps a student product and model to
--   a feedback object.
give_feedback :: EvaluateProduct p m f => p -> m -> f
give_feedback = evaluate_product
-- | Evaluates a batch of student products against the same model.
--   Returns each product paired with its feedback, preserving order.
--   Useful for grading or flagging a whole submission set at once.
give_feedback_batch :: EvaluateProduct p m f => [p] -> m -> [(p, f)]
give_feedback_batch products model =
  map (\p -> (p, evaluate_product p model)) products
-- | Iteratively revises a product until the feedback satisfies a criterion,
--   or a maximum number of evaluation cycles is exhausted.
--   The revision function updates the product given the current feedback.
--   Returns the final (product, feedback) pair and the number of cycles performed.
--

revise_until :: EvaluateProduct p m f
             => (f -> Bool)   -- ^ Termination criterion on the feedback
             -> (p -> f -> p) -- ^ Revision: produce an improved product from feedback
             -> Int           -- ^ Maximum number of evaluation cycles
             -> p -> m -> (p, f, Int)
revise_until satisfied revise max_rounds product model =
  go product 0
  where
    go p n =
      let f = evaluate_product p model
      in  if satisfied f || n + 1 >= max_rounds
            then (p, f, n + 1)
            else go (revise p f) (n + 1)

give_feedback_until :: EvaluateProduct p m f
                    => (f -> Bool)
                    -> (p -> f -> p)
                    -> Int
                    -> p -> m -> (p, f, Int)
give_feedback_until = revise_until
