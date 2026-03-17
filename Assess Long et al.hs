{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AssessLongEtAl where
import Assess
-- | ---------------------------
-- | Assess Implementation: Long et al.
-- | ---------------------------
-- | Long, T., Qin, J., Shen, J., Zhang, W., Xia, W., Tang, R., He, X., & Yu, Y. (2022).
-- | Improving Knowledge Tracing with Collaborative Information. Proceedings of the
-- | 15th ACM International Conference on Web Search and Data Mining (WSDM '22),
-- | pp. 1054-1062. https://doi.org/10.1145/3488560.3498374
-- | Collaborative Knowledge Tracing (CoKT) model.
-- | The system selects the most informative question for the learner.
-- | The response is interpreted by fusing:
-- |   intra-student information: the learner's own question-answering history X_{t-1}^u
-- |   inter-student information: similar peer records R_t^u and similar peer
-- |   sub-sequences S_t^u, retrieved via BM25 over historical strings (Eq. 3).
data CoKTModel = CoKTModel { questionBank :: [Question]
                           , database     :: [Record]
                           } deriving (Show)
-- | A question q_i with its associated concept set C_i (Definition 3.2)
data Question  = Question  { questionId :: Int
                           , conceptIds :: [Int]
                           } deriving (Show, Eq)
-- | A record r_i^u = {q_i, C_i, y_i}: the question, its concepts, and
-- | the student's correctness y_i (Definition 3.2).
data Record    = Record    { studentId  :: Int
                           , question   :: Question
                           , correctness :: Bool
                           } deriving (Show, Eq)
-- | A student u with her question-answering history X_{t-1}^u (Definition 3.3)
data Student   = Student   { stuId   :: Int
                           , history :: [Record]
                           } deriving (Show, Eq)
-- | The student's response y_t to the presented question q_t
data Response  = Response  { respQuestion :: Question
                           , respCorrect  :: Bool
                           } deriving (Show, Eq)
-- | The predicted probability y^_t = Pr(y_t=1 | q_t, C_t, X_{t-1}^u, S_t^u, R_t^u)
-- | and the updated knowledge state h_t (Definition 3.1)
data MasteryEstimate = MasteryEstimate { predictedProb  :: Double
                                       , knowledgeState :: [Double]
                                       } deriving (Show, Eq, Ord)
-- SelectItem instance: the system selects the most informative next question
-- q_t from the question bank for this student.
instance SelectItem Student CoKTModel Question where
  select_item student model = undefined
-- InterpretResponse instance: retrieves similar peer records R_t^u and
-- sub-sequences S_t^u from the database via BM25 (Eq. 3), fuses
-- inter-student and intra-student information (Eq. 19), and predicts
-- y^_t via the prediction module (Eq. 20), then updates h_t (Eq. 22).
instance InterpretResponse Student CoKTModel Question Response MasteryEstimate where
  interpret student model item response = undefined
-- The (Question -> Response) argument represents the learner
-- actually responding to the presented question q_t.
service_cokt :: Student -> CoKTModel -> (Question -> Response) -> MasteryEstimate
service_cokt = assess