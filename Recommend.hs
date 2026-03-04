{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
module Recommend where

import Data.List ((\\))

-- | -----------------------------------------------------------------------
-- | Recommend Service
-- | -----------------------------------------------------------------------
-- | Definition: recommends a learning activity based on learner behaviour,
-- | a learner profile, a learner model, and/or a peer-learner model.
-- | The receiver (student, teacher, or researcher) retains the choice to
-- | follow the recommendation or not.
-- |
-- | Because recommendation requires two distinct steps (finding candidates,
-- | then ranking them), we use two type classes to form a pipeline.
-- | -----------------------------------------------------------------------

-- This is a generalized approach to modeling services using type classes.
learning_service  ::  (Model m l) => i -> m -> o
learning_service  =   undefined

-- | Model is a type class for representing learner and peer-learning models.
-- | Peer-learning models are typically constructed using some init
-- | function that constructs a model from existing data.
-- | A learner model can be updated by means of a learning activity,
-- | represented by a value of type 'l'.
class Model m l where
  init    ::  m
  update  ::  l -> m -> m

-- | Candidates is a type class for determining a set of candidate objects.
-- | 'i' is the input, 'c' is the candidate object type.
class Candidates i c where
  candidates  ::  i -> [c]

-- | Best is a type class for selecting the best object from a set of candidates.
-- | Note: The 'i' (input) is included here so that 'best' has access
-- | to the original context (e.g., the specific Student).
class Best i m c where
  best  ::  i -> m -> [c] -> c

-- | The generic 'recommend' function. It combines candidate selection
-- | and the best object selection into a single pipeline.
-- | Model m l is added to ensure that a properly structured model is present.
recommend              ::  (Model m l, Candidates i c, Best i m c) => i -> m -> c
recommend input model  =   best input model (candidates input)

-- | Convenience: return only the top-k recommendations from a ranked list.
recommendTopK              ::  (Model m l, Candidates i c, Best i m c) => Int -> i -> m -> [c]
recommendTopK k input model =  take k (candidates input)

-- | ---------------------------
-- | General Types
-- | ---------------------------

type Identifier  =  String

-- | A grade can be thought of as a predicted probability of success (0.0 to 1.0).
data Grade       =  Grade Double deriving (Ord, Eq, Show)
data Course      =  Course  { cname   :: Identifier } deriving (Show, Eq, Ord)
data Student     =  Student { sname   :: Identifier
                            , results :: [(Course, Grade)]
                            } deriving (Show, Eq)
data Program     =  Program { courses :: [Course] } deriving (Show)

-- | ---------------------------
-- | Shared Candidate Logic
-- | ---------------------------

instance Candidates (Student, Program) Course where
  -- Input: a tuple of a student and a program.
  -- Output: a list of courses from the program the student has not yet taken.
  candidates (student, program)  =  courses program \\ map fst (results student)
