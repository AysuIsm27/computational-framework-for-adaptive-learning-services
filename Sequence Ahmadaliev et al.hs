{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SequenceAhmadalievEtAl where

import Sequence
import Data.List (sortBy)
import Data.Ord  (comparing)

-- | ---------------------------
-- | Sequence Implementation: Ahmadaliev et al.
-- | ---------------------------
-- | Knowledge-Based Sequencing Model.
-- | The system determines exactly one next activity — the student has no choice.
-- | Ahmadaliev, D., Xiaohui, C., Abduvohidov, M., Medatov, A., & Temirova, G. (2019).
-- | An adaptive activity sequencing instrument to enhance e-learning.
-- | 2019 International Conference on Computer and Information Sciences (ICCIS), 1–4.
-- | DOI: https://doi.org/10.1109/ICCISci.2019.8716473

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

-- Looks up the prerequisites for a topic in the knowledge graph.
prerequisites :: SequencingModel -> Topic -> [Topic]
prerequisites model topic =
  case lookup topic (prerequisiteGraph model) of
    Just ps -> ps
    Nothing -> []

-- Looks up the difficulty level assigned to a topic.
topicDifficulty :: SequencingModel -> Topic -> Int
topicDifficulty model topic =
  case lookup topic (difficultyLevels model) of
    Just d  -> d
    Nothing -> 0

-- Returns True if every prerequisite of the topic has been mastered.
prerequisitesMastered :: SequencingModel -> StudentState -> Topic -> Bool
prerequisitesMastered model student topic =
  all (`elem` masteredTopics student) (prerequisites model topic)

-- The unlocked frontier: topics whose prerequisites are all mastered
-- but which the student has not yet mastered themselves.
availableTopics :: SequencingModel -> StudentState -> [Topic]
availableTopics model student =
  [ t | (t, _) <- prerequisiteGraph model
      , t `notElem` masteredTopics student
      , prerequisitesMastered model student t ]

-- Retrieves the first activity in the bank that covers a given topic.
activityForTopic :: [LearningActivity] -> Topic -> LearningActivity
activityForTopic bank topic =
  case filter (\a -> activityTopic a == topic) bank of
    (a:_) -> a
    []    -> undefined

allActivities :: [LearningActivity]
allActivities = undefined

-- DetermineNext instance: selects the unlocked topic with the lowest
-- difficulty and returns its corresponding learning activity.
instance DetermineNext StudentState SequencingModel LearningActivity where
  determine_next student model =
    let available = availableTopics model student
        sorted    = sortBy (comparing (topicDifficulty model)) available
    in  case sorted of
          (t:_) -> activityForTopic allActivities t
          []    -> undefined

service_sequence :: StudentState -> SequencingModel -> LearningActivity
service_sequence =  sequence_activity

-- | -----------------------------------------------------------------------
-- | Multi-Step Simulation (Using sequence_path from the Sequence.hs)
-- | -----------------------------------------------------------------------

-- | Transition function: updates the student state after completing an activity.
--   This allows sequence_path to calculate the step after this one.
simulate_learning :: StudentState -> LearningActivity -> StudentState
simulate_learning state activity = 
  let completedTopic = activityTopic activity
  in  state { masteredTopics = completedTopic : masteredTopics state }

-- | Generates a multi-step lesson plan of length 'n'
generate_lesson_plan :: Int -> StudentState -> SequencingModel -> [LearningActivity]
generate_lesson_plan steps student model = 
  sequence_path simulate_learning steps student model
