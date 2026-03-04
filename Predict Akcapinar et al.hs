{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module PredictAkcapinarEtAl where

import Predict

-- | ---------------------------
-- | Predict Implementation: Akcapinar et al.
-- | ---------------------------
-- | kNN-based early-warning system using Moodle LMS interaction features.
-- | Best classifier from a comparison of 7 algorithms (NaiveBayes, CART,
-- | RandomForest, SVM, NeuralNet, CN2, kNN). kNN achieved 89% accuracy.
-- | Akçapınar, G., Altun, A., & Aşkar, P. (2019).
-- | Using learning analytics to develop early-warning system for at-risk students.
-- | International Journal of Educational Technology in Higher Education, 16, 40.
-- | DOI: https://doi.org/10.1186/s41239-019-0172-z

data PredictionModel = PredictionModel
  { trainingSet :: [(ActivityLog, Bool)]  -- ^ (log, True=passed)
  , kNeighbours :: Int                    -- ^ k in kNN
  } deriving (Show)

data ActivityLog = ActivityLog
  { logStudentId      :: Int
  , numSessions       :: Int   -- ^ number of login sessions
  , resourceViews     :: Int   -- ^ number of resource/material views
  , forumPosts        :: Int   -- ^ number of forum posts (active)
  , forumReads        :: Int   -- ^ number of forum reads (passive)
  , assignmentSubmits :: Int   -- ^ number of assignment submissions
  , quizAttempts      :: Int   -- ^ number of quiz attempts
  } deriving (Show, Eq)

data Outcome = Outcome
  { willPass  :: Bool
  , riskLevel :: RiskLevel
  } deriving (Show, Eq)

data RiskLevel = LowRisk | HighRisk deriving (Show, Eq)

-- | PredictObservable instance: maps the learner's early-course Moodle
-- | activity log to a predicted pass/fail outcome via kNN.
instance PredictObservable ActivityLog PredictionModel Outcome where
  predict_result log model = undefined

service_predict :: ActivityLog -> PredictionModel -> Outcome
service_predict = predict_service
