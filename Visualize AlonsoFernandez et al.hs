{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module VisualizeAlonsoFernandezEtAl where

import Visualize
import Data.List (minimumBy)
import Data.Ord  (comparing)

-- | ---------------------------
-- | Visualize Implementation: Alonso-Fernández et al.
-- | ---------------------------
-- | T-Mon Monitoring Model.
-- | Aggregates xAPI-SG game traces into per-student clusters and engagement
-- | scores, flags the most at-risk student, and renders a teacher dashboard.

data TMonModel = TMonModel { modelDataTMon :: String
                           } deriving (Show)

data GameTrace = GameTrace { studentName :: String
                           , actionTaken :: String
                           , timeOfEvent :: Double
                           } deriving (Show, Eq)

data GameContext = GameContext { classId   :: String
                               , allTraces :: [GameTrace]
                               } deriving (Show, Eq)

data Dashboard = Dashboard { progressCharts  :: [String]
                           , timeGraphs      :: [String]
                           , clusterPlots    :: [String]
                           , summaryWarnings :: String
                           } deriving (Show, Eq)

newtype Cluster = Cluster { clusterId :: Int } deriving (Show, Eq, Ord)

predict_student_cluster                              :: String -> TMonModel -> [GameTrace] -> Cluster
predict_student_cluster student model studentTraces  =  undefined

studentEngagement    :: String -> Double
studentEngagement _  =  undefined

groupByStudent    :: [GameTrace] -> [(String, [GameTrace])]
groupByStudent _  =  undefined

-- RenderVisualization instance: computes play style clusters and engagement
-- scores, identifies the most at-risk student, and renders the dashboard.
instance RenderVisualization GameContext TMonModel Dashboard where
  render (GameContext _ traces) model =
    let studentTracePairs  =  groupByStudent traces
        predictedClusters  =  map (\(s, ts) -> (s, predict_student_cluster s model ts)) studentTracePairs
        engagementScores   =  map (\(s, _)  -> (s, studentEngagement s))               studentTracePairs
        (mostAtRisk, _)    =  minimumBy (comparing snd) engagementScores
    in  Dashboard { progressCharts  = undefined
                  , timeGraphs      = undefined
                  , clusterPlots    = map (\(s, c) -> s ++ ": cluster " ++ show (clusterId c)) predictedClusters
                  , summaryWarnings = "At risk: " ++ mostAtRisk
                  }

service_tmon  :: GameContext -> TMonModel -> Dashboard
service_tmon  =  visualize
