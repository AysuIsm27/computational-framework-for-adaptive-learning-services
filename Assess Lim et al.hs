{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AssessLimEtAl where

import Assess

-- | ---------------------------
-- | Assess Implementation: Lim et al.
-- | ---------------------------
-- | Gamified Hybrid Multimodal Assessment (GHMA) Model.
-- | The system selects the challenge; the learner does not choose.
-- | The response is interpreted as evidence about the learner's mastery state.

data GHMAModel = GHMAModel { gamificationElements :: [(String, Int)]
                           , learningPaths        :: [(String, [AssessmentChallenge])]
                           , allowedTools         :: [Modality]
                           , pastData             :: [(Int, [Double])]
                           } deriving (Show)

data Modality = Video | LivePresentation | WebPublication | Report | RolePlay
                deriving (Show, Eq)

data Learner = Learner { learnerId :: Int
                       , skills    :: [String]
                       } deriving (Show, Eq)

data AssessmentChallenge = AssessmentChallenge { challengeId :: Int
                                               , difficulty  :: Int
                                               , modality    :: Modality
                                               } deriving (Show, Eq)

data LearnerResponse = LearnerResponse { responseLearnerId  :: Int
                                       , attemptedChallenge :: AssessmentChallenge
                                       , createdWork        :: Modality
                                       , trackingData       :: [Double]
                                       } deriving (Show, Eq)

data AssessmentResult = AssessmentResult { masteryEstimate :: Double
                                         , diagnosticInfo  :: String
                                         } deriving (Show, Eq, Ord)

-- SelectItem instance: the system selects the challenge best matched to
-- the learner's current level. The learner does not choose.
instance SelectItem Learner GHMAModel AssessmentChallenge where
  select_item learner model  =  undefined

-- InterpretResponse instance: interprets the learner's response as evidence
-- about their mastery state, returning a mastery estimate and diagnostic info.
instance InterpretResponse Learner GHMAModel AssessmentChallenge LearnerResponse AssessmentResult where
  interpret learner model item response  =  undefined

-- The (AssessmentChallenge -> LearnerResponse) argument represents the
-- learner actually responding to the presented challenge.
service_ghma  :: Learner -> GHMAModel -> (AssessmentChallenge -> LearnerResponse) -> AssessmentResult
service_ghma  =  assess
