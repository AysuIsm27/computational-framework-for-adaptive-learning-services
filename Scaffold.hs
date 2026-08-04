{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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
-- | attention, model strategies, or guide learner actions. The support is
-- | gradually withdrawn as the learner becomes more competent and takes over
-- | responsibility for the task.
-- |
-- | Because scaffolding requires two distinct steps (determining the support
-- | that is available, then deciding how much of it is due), we use a type
-- | class for available support and a standalone contingency function per
-- | implementation. Unlike a hint service, the support is applied to the task
-- | itself, so the service returns a task state rather than a clue.
-- |
-- | 't' is the learner's current task state, 'm' is the learner model,
-- | peer-learner model, or combination of models, and 's' is a scaffold.
-- | -----------------------------------------------------------------------

-- | Model is a type class for representing learner and peer-learning models.
-- | A learner model can be updated by means of a learning activity,
-- | represented by a value of type 'l'.
class Model m l | m -> l where
  initial ::  m
  update  ::  l -> m -> m

-- | AvailableScaffolds is a type class for determining the support a system
-- | can offer for a task state.
class AvailableScaffolds t m s | t m -> s where
  available_scaffolds  ::  t -> m -> [s]

-- | The generic 'scaffold' function. It obtains the available support and
-- | delegates the contingency decision to a caller-supplied function, which
-- | returns the scaffold that is due, or Nothing when the learner needs none.
scaffold  ::  AvailableScaffolds t m s
          =>  (t -> m -> [s] -> Maybe s)  -- ^ contingency
          ->  t -> m -> Maybe s
scaffold contingent task model = contingent task model (available_scaffolds task model)

-- | Applies the scaffold that is due to the task, leaving the task unchanged
-- | when no support is due. This is the service proper: a task state in, an
-- | adapted task state out.
scaffold_task  ::  AvailableScaffolds t m s
               =>  (t -> m -> [s] -> Maybe s)  -- ^ contingency
               ->  (s -> t -> t)               -- ^ how a scaffold reshapes the task
               ->  t -> m -> t
scaffold_task contingent apply_scaffold task model =
  maybe task (`apply_scaffold` task) (scaffold contingent task model)

-- | Generates scaffolds for a batch of task states using the same learner
--   model. Returns each task state paired with its scaffold, preserving order.
scaffold_batch  ::  AvailableScaffolds t m s
                =>  (t -> m -> [s] -> Maybe s) -> [t] -> m -> [(t, Maybe s)]
scaffold_batch contingent tasks model =
  map (\t -> (t, scaffold contingent t model)) tasks

-- | Iteratively applies scaffolding until the learner can proceed
--   independently, or a maximum number of scaffold cycles is exhausted.
--   Each cycle updates the learner model with the learner's engagement in the
--   scaffolded task, so support is withdrawn as competence is recorded.
--   Returns the final task state, the final model, the scaffolds applied in
--   order, and the number of cycles.
scaffold_until_independent  ::  (AvailableScaffolds t m s, Model m l)
                            =>  (t -> m -> [s] -> Maybe s)  -- ^ contingency
                            ->  (s -> t -> t)               -- ^ how a scaffold reshapes the task
                            ->  (t -> l)                    -- ^ the learner's engagement in the task
                            ->  Int                         -- ^ maximum number of scaffold cycles
                            ->  t -> m -> (t, m, [s], Int)
scaffold_until_independent contingent apply_scaffold engage max_rounds task model
  | max_rounds <= 0  =  (task, model, [], 0)
  | otherwise        =  go task model [] 0
  where
    go t m applied n =
      case scaffold contingent t m of
        Nothing  ->  (t, m, reverse applied, n)          -- support is no longer due
        Just s   ->
          let  t'  =  apply_scaffold s t
               m'  =  update (engage t') m
               n'  =  n + 1
          in   if n' >= max_rounds
                 then (t', m', reverse (s : applied), n')
                 else go t' m' (s : applied) n'

-- | Whether responsibility for the task has passed to the learner: no
-- | scaffold is due for the current task state.
independent  ::  AvailableScaffolds t m s
             =>  (t -> m -> [s] -> Maybe s) -> t -> m -> Bool
independent contingent task model =
  case scaffold contingent task model of
    Nothing  ->  True
    Just _   ->  False
