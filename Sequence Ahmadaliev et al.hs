{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module SequenceAhmadalievEtAl where

import Sequence

-- | ---------------------------
-- | Sequence Implementation: Ahmadaliev et al.
-- | ---------------------------
-- | Adaptive activity sequencing via weighted overlay user model and
-- | relaxing-based mathematical programming.
-- | LO selection is driven by knowledge level y_c = Σ Q_{i,j} × x_{i,j}
-- | against a fixed MinQ=70 threshold, with learning mode (sensing/intuitive)
-- | updated by Algorithm 1 (Felder-Silverman perception dimension).
-- | Ahmadaliev, D., Xiaohui, C., Abduvohidov, M., Medatov, A., & Temirova, G. (2019).
-- | An adaptive activity sequencing instrument to enhance e-learning.
-- | 2019 International Conference on Computer and Information Sciences (ICCIS), 1–4.
-- | DOI: https://doi.org/10.1109/ICCISci.2019.8716473

-- | Minimum quality pass value: hard-coded in the paper as MinQ = 70.
minQ :: Double
minQ = 70.0

-- | Learning mode from Felder-Silverman perception dimension (Algorithm 1).
-- | Sensing:   easy/concrete LOs — default after a fail.
-- | Intuitive: abstract/challenging LOs (higher award) — default for new user.
data LearningMode = Sensing | Intuitive deriving (Show, Eq)

-- | A Learning Object (LO) as described in the paper (Section III).
-- | Each LO has: title, content, problem, variant answers (for quiz LOs),
-- | a weight w assigned by the educator, and a mode (sensing/intuitive).
-- | Weights per sub-theme sum to 100 (equation 3: Q_i = [lo_{i,:,w}]).
data LearningObject = LearningObject
  { loId     :: String       -- ^ identifier
  , loWeight :: Double       -- ^ weight w; weights in sub-theme sum to 100
  , loMode   :: LearningMode -- ^ sensing (easy) or intuitive (abstract/challenging)
  } deriving (Show, Eq)

-- | Domain model: three-level hierarchy Subject → Theme → Sub-theme → LO (Fig. 1).
-- | allLOs !! j !! s = list of LOs in theme j, sub-theme s.
-- | Corresponds to LO_{i,j} and CL_i in equations (1) and (2).
data SequencingModel = SequencingModel
  { allLOs :: [[[LearningObject]]] -- ^ theme → sub-theme → [LO]
  } deriving (Show)

-- | Student state: the weighted overlay user model.
-- | Tracks CT (taken LOs), y_c (knowledge level), current position,
-- | and learning mode with comprehension counter (Algorithm 1).
-- | New user default mode: Intuitive (Algorithm 1, Step 1).
data StudentState = StudentState
  { studentId            :: Int
  , currentTheme         :: Int           -- ^ j index into allLOs
  , currentSubTheme      :: Int           -- ^ sub-theme index within theme j
  , takenLOs             :: [String]      -- ^ CT: ids of completed LOs (equation 4)
  , knowledgeLevel       :: Double        -- ^ y_c = Σ Q_{i,j} * x_{i,j} (equation 6)
  , learningMode         :: LearningMode  -- ^ Intuitive for new user (Algorithm 1, Step 1)
  , comprehensionCounter :: Int           -- ^ consecutive passes in sensing; 3 → intuitive
  } deriving (Show, Eq)

-- | DetermineNext instance: implements Algorithm 2 from the paper.
-- | Step 1: compute y_c over all TokenLOs in CurrentSubTheme (equation 6)
-- | Step 2: if y_c >= MinQ, advance to next sub-theme (equation 7)
-- | Step 3/4: collect untaken LOs from CurrentSubTheme
-- | Step 5: return the LO whose mode matches the student's current learning mode
instance DetermineNext StudentState SequencingModel LearningObject where
  determine_next student model = undefined

service_sequence :: StudentState -> SequencingModel -> LearningObject
service_sequence = sequence_activity

-- | Transition: implements Algorithm 1 (evaluate user action) from the paper.
-- | passed=True:  if mode=Sensing, increment counter; if counter=3 → Intuitive
-- | passed=False: reset counter to 0, switch mode to Sensing
-- | Both: add completed LO to takenLOs, update knowledgeLevel y_c
simulate_learning :: Bool -> StudentState -> LearningObject -> StudentState
simulate_learning passed student lo = undefined

-- | Generates a multi-step lesson plan of length n assuming all LOs are passed.
generate_lesson_plan :: Int -> StudentState -> SequencingModel -> [LearningObject]
generate_lesson_plan steps student model =
  sequence_path (simulate_learning True) steps student model
