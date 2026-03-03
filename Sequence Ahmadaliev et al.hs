{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SequenceAhmadalievEtAl where

import Sequence

-- | ---------------------------
-- | Sequence Implementation: Ahmadaliev et al.
-- | ---------------------------
-- | Knowledge-Based Sequencing Model.
-- | The system determines exactly one next activity — the student has no choice.

data SequencingModel = SequencingModel { prerequisiteGraph :: [(Topic, [Topic])]
                                       , difficultyLevels  :: [(Topic, Int)]
                                       } deriving (Show)

newtype Topic = Topic { topicId :: String } deriving (Show, Eq, Ord)

data StudentState = StudentState { stateStudentId :: Int
                                 , masteredTopics :: [Topic]
                                 , attemptHistory :: [(Topic, Double)]
                                 } deriving (Show, Eq)

data LearningActivity = LearningActivity { activityTopic      :: Topic
                                         , activityDifficulty :: Int
                                         , activityType       :: ActivityType
                                         } deriving (Show, Eq)

data ActivityType = Explanation | Practice | Quiz deriving (Show, Eq)

-- DetermineNext instance: maps the student's current state directly to
-- exactly one next activity. No alternatives are offered.
instance DetermineNext StudentState SequencingModel LearningActivity where
  determine_next student model  =  undefined

service_sequence  :: StudentState -> SequencingModel -> LearningActivity
service_sequence  =  sequence_activity
