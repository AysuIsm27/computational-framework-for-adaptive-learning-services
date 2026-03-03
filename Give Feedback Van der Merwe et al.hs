{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module GiveFeedbackVanDerMerweEtAl where

import GiveFeedback

-- | ---------------------------
-- | Give Feedback Implementation: Van der Merwe et al.
-- | ---------------------------
-- | Data Envelopment Analysis (DEA) Feedback Model.
-- | Evaluates a student's grades against the class efficient frontier
-- | and returns specific improvement targets.

data DEAModel = DEAModel { classData :: [StudentGrades]
                         } deriving (Show)

data StudentGrades = StudentGrades { studentId :: Int
                                   , scores    :: [Double]
                                   } deriving (Show, Eq)

data FeedbackTarget = FeedbackTarget { efficiencyScore   :: Double
                                     , targetScores      :: [Double]
                                     , improvementAdvice :: String
                                     } deriving (Show, Eq)

-- EvaluateProduct instance: evaluates the student's submitted grades against
-- the class efficient frontier and produces specific improvement targets.
-- Uses linear programming to compute the shortest path to the frontier.
instance EvaluateProduct StudentGrades DEAModel FeedbackTarget where
  evaluate_product student model  =  undefined

service_dea  :: StudentGrades -> DEAModel -> FeedbackTarget
service_dea  =  give_feedback
