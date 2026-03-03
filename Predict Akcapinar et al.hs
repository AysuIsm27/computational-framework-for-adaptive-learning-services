{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module PredictAkcapinarEtAl where

import Predict

-- | ---------------------------
-- | Predict Implementation: Akcapinar et al.
-- | ---------------------------
-- | Learning Analytics Early-Warning Prediction Model.
-- | Extracts early-course LMS behavioural features and applies logistic
-- | regression to predict whether a student will pass the course.
-- | Akçapınar, G., Altun, A., & Aşkar, P. (2019).
-- | Using learning analytics to develop early-warning system for at-risk students.
-- | International Journal of Educational Technology in Higher Education, 16, 40.
-- | DOI: https://doi.org/10.1186/s41239-019-0172-z

data PredictionModel = PredictionModel { modelWeights :: [Double]
                                       , threshold    :: Double
                                       } deriving (Show)

data ActivityLog = ActivityLog { logStudentId   :: Int
                               , loginFrequency :: Int
                               , resourceViews  :: Int
                               , forumPosts     :: Int
                               , assignmentsDue :: [Bool]
                               } deriving (Show, Eq)

data Outcome = Outcome { passProbability :: Double
                       , riskLevel       :: RiskLevel
                       } deriving (Show, Eq)

data RiskLevel = LowRisk | MediumRisk | HighRisk deriving (Show, Eq)

-- PredictObservable instance: maps the learner's early-course activity log
-- directly to a predicted pass/fail outcome and risk level.
instance PredictObservable ActivityLog PredictionModel Outcome where
  predict_result log model  =  undefined

service_predict  :: ActivityLog -> PredictionModel -> Outcome
service_predict  =  predict_service
