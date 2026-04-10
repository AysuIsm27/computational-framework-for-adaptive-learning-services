{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RecommendImplementations where

import Recommend
import Data.List (sortBy, (\\))
import Data.Ord  (comparing, Down(..))

-- | -----------------------------------------------------------------------
-- | Recommend Implementations
-- | -----------------------------------------------------------------------
-- | All three papers below implement the Candidates typeclass to determine
-- | eligible items for a learner, and provide a standalone rank function
-- | to order them from most to least suitable.
-- | -----------------------------------------------------------------------


-- | =======================================================================
-- | 1. Chen et al. — Item Response Theory (IRT)
-- | =======================================================================
-- | Chen, C. M., Liu, C. Y., & Chang, M. H. (2006).
-- | Personalized curriculum sequencing utilizing modified item response
-- | theory for web-based instruction.
-- | Expert Systems with Applications, 30(2), 378-396.
-- | DOI: https://doi.org/10.1016/j.eswa.2005.07.029
-- | Recommends items whose difficulty best matches the learner's estimated
-- | ability (theta). The learner may choose which item to attempt.

data IRTModel = IRTModel { itemParameters   :: [(Item, IRTParams)]
                         , abilityEstimates :: [(Int, Double)]
                         } deriving (Show)

data IRTParams = IRTParams { difficulty     :: Double  -- b parameter
                           , discrimination :: Double  -- a parameter
                           , guessing       :: Double  -- c parameter
                           } deriving (Show, Eq)

data Item = Item { itemId      :: Int
                 , itemContent :: String
                 } deriving (Show, Eq)

data IRTLearner = IRTLearner { irtStudentId    :: Int
                             , responseHistory :: [(Item, Bool)]
                             } deriving (Show, Eq)

-- Estimates the learner's current ability (theta) from their response history.
estimateAbility    :: IRTLearner -> Double
estimateAbility _  =  undefined

-- Looks up the difficulty parameter for a given item from the IRT model.
itemDifficulty            :: IRTModel -> Item -> Double
itemDifficulty model item  =
  case lookup item (itemParameters model) of
    Just p  -> difficulty p
    Nothing -> 0.0

allItems  :: [Item]
allItems  =  undefined

-- Candidates: all items the learner has not yet attempted.
instance Candidates IRTLearner IRTModel Item where
  candidates learner _ =
    let attempted = map fst (responseHistory learner)
    in  allItems \\ attempted

-- Rank: orders candidates by smallest gap between item difficulty and
-- the learner's estimated ability.
rank_irt :: IRTLearner -> IRTModel -> [Item] -> [Item]
rank_irt learner model cs =
  let theta = estimateAbility learner
  in  sortBy (comparing (\c -> abs (itemDifficulty model c - theta))) cs

service_irt :: IRTLearner -> IRTModel -> [Item]
service_irt learner model =
  let cs = candidates learner model
  in  rank_irt learner model cs


-- | =======================================================================
-- | 2. Jagan et al. -- Latent Dirichlet Allocation (LDA)
-- | =======================================================================
-- | Jagan, A., Nagarajan, V., & Sathiyamurthy, K. (2017).
-- | Latent Dirichlet allocation based behavior identification system
-- | for personalized E-content generation.
-- | DOI: 10.1166/jctn.2016.5956
-- | Recommends resources ranked by topic similarity to the learner's
-- | inferred behavioural topic mixture. The learner may choose which to access.

data LDAModel = LDAModel { topicDistributions :: [[Double]]
                         , numTopics          :: Int
                         } deriving (Show)

data InteractionLog = InteractionLog { logStudentId  :: Int
                                     , logResourceId :: String
                                     , duration      :: Double
                                     , actionType    :: String
                                     } deriving (Show, Eq)

data LDALearner = LDALearner { ldaLearnerId       :: Int
                             , interactionHistory :: [InteractionLog]
                             } deriving (Show, Eq)

data Resource = Resource { resId        :: String
                         , resType      :: String
                         , resTopicDist :: [Double]
                         } deriving (Show, Eq)

-- Infers the learner's topic mixture from their interaction history.
inferTopicMixture               :: LDAModel -> LDALearner -> [Double]
inferTopicMixture model learner  =  undefined

-- Dot product similarity between two topic distributions.
topicSimilarity        :: [Double] -> [Double] -> Double
topicSimilarity xs ys  =  sum (zipWith (*) xs ys)

allResources  :: [Resource]
allResources  =  undefined

-- Candidates: all resources the learner has not yet accessed.
instance Candidates LDALearner LDAModel Resource where
  candidates learner _ =
    let accessed = map logResourceId (interactionHistory learner)
    in  filter (\r -> resId r `notElem` accessed) allResources

-- Rank: orders candidates by topic similarity to the learner's topic mixture.
rank_lda :: LDALearner -> LDAModel -> [Resource] -> [Resource]
rank_lda learner model cs =
  let topicMix = inferTopicMixture model learner
  in  sortBy (comparing (Down . topicSimilarity topicMix . resTopicDist)) cs

service_lda :: LDALearner -> LDAModel -> [Resource]
service_lda learner model =
  let cs = candidates learner model
  in  rank_lda learner model cs


-- | =======================================================================
-- | 3. Rodriguez-Martinez et al. -- Formative Assessment
-- | =======================================================================
-- | Rodriguez-Martinez, J. A., Gonzalez-Calero, J. A., del Olmo-Munoz, J.,
-- | Arnau, D., & Tirado-Olivares, S. (2023).
-- | Building personalised homework from a learning analytics based formative
-- | assessment: Effect on fifth-grade students' understanding of fractions.
-- | British Journal of Educational Technology, 54(1), 88-108.
-- | DOI: https://doi.org/10.1111/bjet.13292
-- | Recommends fraction tasks ranked by weakest skill first, then by
-- | increasing difficulty. The learner may choose which task to attempt.

data FormativeModel = FormativeModel { taskBank        :: [FractionTask]
                                     , performanceData :: [(Int, [TaskResult])]
                                     } deriving (Show)

data FractionTask = FractionTask { taskId         :: Int
                                 , taskSkill      :: FractionSkill
                                 , taskDifficulty :: Int
                                 } deriving (Show, Eq)

data FractionSkill = AddFractions
                   | SubtractFractions
                   | MultiplyFractions
                   | DivideFractions
                   | CompareFractions
                   | SimplifyFractions
                   deriving (Show, Eq, Ord, Enum, Bounded)

data TaskResult = TaskResult { resultTaskId :: Int
                             , correct      :: Bool
                             , timeSpent    :: Double
                             } deriving (Show, Eq)

data FormativeLearner = FormativeLearner { formativeStudentId :: Int
                                         , taskHistory        :: [TaskResult]
                                         } deriving (Show, Eq)

-- Computes mastery score for a given fraction skill from the learner's history.
skillMastery                      :: FormativeModel -> FormativeLearner -> FractionSkill -> Double
skillMastery model learner skill  =
  let relevantIds  = [ taskId t | t <- taskBank model, taskSkill t == skill ]
      results      = [ r | r <- taskHistory learner, resultTaskId r `elem` relevantIds ]
      correctCount = length (filter correct results)
  in  if null results then 0.0 else fromIntegral correctCount / fromIntegral (length results)

-- Candidates: all tasks the learner has not yet attempted.
instance Candidates FormativeLearner FormativeModel FractionTask where
  candidates learner model =
    let attempted = map resultTaskId (taskHistory learner)
    in  filter (\t -> taskId t `notElem` attempted) (taskBank model)

-- Rank: prioritises tasks targeting the learner's weakest skill, then
-- orders by increasing difficulty within that skill.
rank_formative :: FormativeLearner -> FormativeModel -> [FractionTask] -> [FractionTask]
rank_formative learner model cs =
  sortBy (\a b -> compare (skillMastery model learner (taskSkill a), taskDifficulty a)
                          (skillMastery model learner (taskSkill b), taskDifficulty b)) cs

service_formative :: FormativeLearner -> FormativeModel -> [FractionTask]
service_formative learner model =
  let cs = candidates learner model
  in  rank_formative learner model cs
