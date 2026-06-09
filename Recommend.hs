{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Recommend where

import Data.List ((\\))

-- | Model is a type class for representing learner and peer-learning models.
-- | The '| m -> l' ensures the compiler knows exactly which activity updates the model.
class Model m l | m -> l where
  init    ::  m
  update  ::  l -> m -> m

-- | Candidates is a type class for determining a set of candidate objects.
-- | We pass 'm' (the model) so candidate selection knows the model's context.
class Candidates i m c where
  candidates  ::  i -> m -> [c]

-- | The generic 'recommend' function. 
-- | It forces the compiler to verify BOTH the Candidates and Model instances exist.
recommend :: (Candidates i m c, Model m l) => (i -> m -> [c] -> [c]) -> i -> m -> [c]
recommend rank input model = rank input model (candidates input model)

-- | Convenience: return only the top-n recommendations from a ranked list.
recommendTopN :: (Candidates i m c, Model m l) => Int -> (i -> m -> [c] -> [c]) -> i -> m -> [c]
recommendTopN n rank input model = take n (recommend rank input model)

-- | ---------------------------
-- | General Types & Shared Logic
-- | ---------------------------
type Identifier  =  String

data Grade       =  Grade Double deriving (Ord, Eq, Show)
data Course      =  Course  { cname   :: Identifier } deriving (Show, Eq, Ord)
data Student     =  Student { sname   :: Identifier
                            , results :: [(Course, Grade)]
                            } deriving (Show, Eq)
data Program     =  Program { courses :: [Course] } deriving (Show)

instance Candidates Student Program Course where
  candidates student program  =  courses program \\ map fst (results student)
