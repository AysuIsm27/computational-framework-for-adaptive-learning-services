{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module GiveFeedbackPiechEtAl where

import GiveFeedback
import Data.List (sort)

-- | =======================================================================
-- | Piech et al. -- Tuned Models of Peer Assessment in MOOCs
-- | =======================================================================
-- | Piech, C., Huang, J., Chen, Z., Do, C., Ng, A., & Koller, D. (2013).
-- | Tuned models of peer assessment in MOOCs.
-- | arXiv preprint arXiv:1307.2579.
-- | DOI: https://doi.org/10.48550/arXiv.1307.2579

-- | Evaluates a student submission by calibrating raw peer grades against
-- | learned grader biases and reliabilities.  The student receives a
-- | precision-weighted true-score estimate in place of the raw median.

data PeerGradingModel = PeerGradingModel { graderParameters :: [(GraderId, GraderParams)]
                                         } deriving (Show)

newtype GraderId = GraderId { graderId :: Int } deriving (Show, Eq)

data GraderParams = GraderParams { graderBias      :: Double  -- ^ b_v: systematic over/under-scoring tendency
                                 , graderPrecision :: Double  -- ^ tau_v: inverse variance of grader noise
                                 } deriving (Show, Eq)

data Submission = Submission { submissionId   :: Int
                             , receivedGrades :: [(GraderId, Double)]  -- ^ (grader v, observed grade z^v_u)
                             } deriving (Show, Eq)

data PeerFeedback = PeerFeedback { calibratedScore :: Double  -- ^ bias-corrected, precision-weighted estimate
                                 , baselineScore   :: Double  -- ^ median of raw grades (Coursera baseline)
                                 } deriving (Show, Eq)

-- Looks up the learned parameters for a grader.
-- Returns neutral defaults (zero bias, unit precision) if not in the model.
lookupGraderParams              :: PeerGradingModel -> GraderId -> GraderParams
lookupGraderParams model gid    =
  case lookup gid (graderParameters model) of
    Just p  -> p
    Nothing -> GraderParams { graderBias = 0.0, graderPrecision = 1.0 }

-- Baseline score used on the Coursera platform: median of the raw peer grades.
medianGrade            :: [Double] -> Double
medianGrade grades      =
  let sorted = sort grades
      n      = length sorted
      mid    = n `div` 2
  in  if odd n
        then sorted !! mid
        else (sorted !! (mid - 1) + sorted !! mid) / 2.0

-- Calibrated true-score estimate: corrects each grade for grader bias and
-- weights by grader precision, so reliable graders contribute more.
calibrateScore               :: PeerGradingModel -> [(GraderId, Double)] -> Double
calibrateScore model grades   =
  let ps          = map (lookupGraderParams model . fst) grades
      corrected   = zipWith (\(_, z) p -> z - graderBias p)   grades ps
      precisions  = map graderPrecision ps
      totalPrec   = sum precisions
  in  if totalPrec == 0.0
        then 0.0
        else sum (zipWith (*) precisions corrected) / totalPrec

-- Grader bias and precision are fitted offline from historical peer grades
-- using Gibbs sampling (PG1 model); requires probabilistic inference.
inferGraderParams      :: [Submission] -> PeerGradingModel
inferGraderParams _     =  undefined

-- EvaluateProduct instance: returns both the calibrated score and the
-- median baseline so the student can see the effect of calibration.
instance EvaluateProduct Submission PeerGradingModel PeerFeedback where
  evaluate_product submission model =
    let grades = receivedGrades submission
    in  PeerFeedback { calibratedScore = calibrateScore model grades
                     , baselineScore   = medianGrade (map snd grades) }

service_peer_grading  :: Submission -> PeerGradingModel -> PeerFeedback
service_peer_grading  =  give_feedback
