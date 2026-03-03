{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module RecommendImplementations where

import Recommend
import Data.List (sortBy, (\\))
import Data.Ord  (comparing, Down(..))

-- | -----------------------------------------------------------------------
-- | Recommend Implementations
-- | -----------------------------------------------------------------------
-- | All four papers below implement the same two typeclasses:
-- |   Candidates i m c  — determine eligible items for a learner
-- |   Best       i m c  — rank them from most to least suitable
-- | Combined here because they share the same generic interface.
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
  candidates learner model =
    let attempted = map fst (responseHistory learner)
    in  allItems \\ attempted

-- Best: orders candidates by smallest gap between item difficulty and
-- the learner's estimated ability.
instance Best IRTLearner IRTModel Item where
  best learner model cs =
    let theta = estimateAbility learner
    in  sortBy (comparing (\c -> abs (itemDifficulty model c - theta))) cs

service_irt  :: IRTLearner -> IRTModel -> [Item]
service_irt  =  recommend


-- | =======================================================================
-- | 2. Nguyen et al. -- Collaborative Filtering (CF)
-- | =======================================================================
-- | Nguyen, V. A., Nguyen, H. H., Nguyen, D. L., & Le, M. D. (2021).
-- | A course recommendation model for students based on learning outcome.
-- | Education and Information Technologies, 26(5), 5389-5415.
-- | DOI: https://doi.org/10.1007/s10639-021-10524-0
-- | Recommends courses ranked by predicted rating, derived from k nearest
-- | peer learners. The learner may choose which course to enrol in.

data CollaborativeModel = CollaborativeModel { ratingMatrix :: [[Double]]
                                             , similarities :: [[Double]]
                                             } deriving (Show)

data Course = Course { courseId   :: String
                     , courseName :: String
                     , courseArea :: String
                     } deriving (Show, Eq)

data CFLearner = CFLearner { cfLearnerId      :: Int
                           , completedCourses :: [(Course, Double)]
                           } deriving (Show, Eq)

ratingVector               :: CollaborativeModel -> CFLearner -> [Double]
ratingVector model learner  =
  case drop (cfLearnerId learner) (ratingMatrix model) of
    (row:_) -> row
    []      -> []

cosineSimilarity        :: [Double] -> [Double] -> Double
cosineSimilarity xs ys  =
  let dot   = sum (zipWith (*) xs ys)
      normX = sqrt (sum (map (^2) xs))
      normY = sqrt (sum (map (^2) ys))
  in  if normX == 0 || normY == 0 then 0.0 else dot / (normX * normY)

nearestNeighbours                     :: CollaborativeModel -> CFLearner -> [CFLearner] -> [CFLearner]
nearestNeighbours model target peers  =
  let targetVec = ratingVector model target
      scored    = map (\p -> (p, cosineSimilarity targetVec (ratingVector model p))) peers
      sorted    = sortBy (comparing (Down . snd)) scored
  in  map fst (take k sorted)
  where k = 5

predictRating                    :: [CFLearner] -> Course -> Double
predictRating neighbours course  =
  let ratings = [ r | n <- neighbours
                    , (c, r) <- completedCourses n
                    , c == course ]
  in  if null ratings then 0.0 else sum ratings / fromIntegral (length ratings)

allCourses    :: [Course];    allCourses    = undefined
allCFLearners :: [CFLearner]; allCFLearners = undefined

-- Candidates: all courses the learner has not yet completed.
instance Candidates CFLearner CollaborativeModel Course where
  candidates learner model =
    let taken = map fst (completedCourses learner)
    in  allCourses \\ taken

-- Best: orders candidates by predicted rating from nearest peers.
instance Best CFLearner CollaborativeModel Course where
  best learner model cs =
    let peers      = allCFLearners \\ [learner]
        neighbours = nearestNeighbours model learner peers
    in  sortBy (comparing (Down . predictRating neighbours)) cs

service_cf  :: CFLearner -> CollaborativeModel -> [Course]
service_cf  =  recommend


-- | =======================================================================
-- | 3. Jagan et al. -- Latent Dirichlet Allocation (LDA)
-- | =======================================================================
-- | Jagan, A., Nagarajan, V., & Sathiyamurthy, K. (2017).
-- | Latent Dirichlet allocation based behavior identification system
-- | for personalized E-content generation.
-- | Recommends resources ranked by topic similarity to the learner's
-- | inferred behavioural topic mixture. The learner may choose which to access.
     DOI: 10.1166/jctn.2016.5956

data LDAModel = LDAModel { topicDistributions :: [[Double]]
                         , numTopics          :: Int
                         } deriving (Show)

data InteractionLog = InteractionLog { logStudentId  :: Int
                                     , logResourceId :: String
                                     , duration      :: Double
                                     , actionType    :: String
                                     } deriving (Show, Eq)

data LDALearner = LDALearner { ldaLearnerId        :: Int
                             , interactionHistory  :: [InteractionLog]
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
  candidates learner model =
    let accessed = map logResourceId (interactionHistory learner)
    in  filter (\r -> resId r `notElem` accessed) allResources

-- Best: orders candidates by topic similarity to the learner's topic mixture.
instance Best LDALearner LDAModel Resource where
  best learner model cs =
    let topicMix = inferTopicMixture model learner
    in  sortBy (comparing (Down . topicSimilarity topicMix . resTopicDist)) cs

service_lda  :: LDALearner -> LDAModel -> [Resource]
service_lda  =  recommend


-- | =======================================================================
-- | 4. Rodriguez-Martinez et al. -- Formative Assessment
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

-- Best: prioritises tasks targeting the learner's weakest skill, then
-- orders by increasing difficulty within that skill.
instance Best FormativeLearner FormativeModel FractionTask where
  best learner model cs =
    sortBy (\a b -> compare (skillMastery model learner (taskSkill a), taskDifficulty a)
                            (skillMastery model learner (taskSkill b), taskDifficulty b)) cs

service_formative  :: FormativeLearner -> FormativeModel -> [FractionTask]
service_formative  =  recommend
