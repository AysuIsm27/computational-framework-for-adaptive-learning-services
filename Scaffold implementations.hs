{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module ScaffoldImplementations where

import Scaffold



-- | 1. Razzaq & Heffernan -- Task Decomposition

-- | Razzaq, L., & Heffernan, N. T. (2006).
-- | Scaffolding vs. hints in the Assistment System.
-- |
-- | Breaks a difficult task into smaller steps and provides support
-- | when the learner has difficulty.

data Item = Item
  { itemId    :: Int
  , itemText  :: String
  , itemHints :: [String]
  } deriving (Show, Eq)

data Answer = Answer
  { answeredItem  :: Int
  , answerCorrect :: Bool
  } deriving (Show, Eq)

data DecompositionTask = DecompositionTask
  { currentItem  :: Item
  , subItems     :: [Item]
  , hintAsked    :: Bool
  , taskMessages :: [String]
  } deriving (Show, Eq)

data DecompositionModel = DecompositionModel
  { answerHistory :: [Answer]
  } deriving (Show)

data Step
  = SubItem Item
  | Hint String
  deriving (Show, Eq)


-- Model: records the learner's answers.
instance Model DecompositionModel Answer where
  initModel =
    DecompositionModel []

  update answer model =
    model { answerHistory = answer : answerHistory model }


latestAnswer :: Item -> DecompositionModel -> Maybe Answer
latestAnswer item model =
  case filter ((== itemId item) . answeredItem)
              (answerHistory model) of
    (a:_) -> Just a
    []    -> Nothing


-- Available scaffolds: smaller task steps and hints.
instance AvailableScaffolds DecompositionTask DecompositionModel Step where
  available_scaffolds task _ =
       [ SubItem i | i <- subItems task ]
    ++ [ Hint h    | h <- itemHints (currentItem task) ]


-- Selects support after an incorrect answer.
contingency_decomposition
  :: DecompositionTask
  -> DecompositionModel
  -> [Step]
  -> Maybe Step
contingency_decomposition task model available =
  case latestAnswer (currentItem task) model of
    Just a | not (answerCorrect a) ->
      if hintAsked task
        then first [ h | h@(Hint _) <- available ]
        else first [ s | s@(SubItem _) <- available ]

    _ ->
      Nothing


-- Applies the selected support to the task.
apply_decomposition :: Step -> DecompositionTask -> DecompositionTask
apply_decomposition (SubItem item) task =
  task
    { currentItem = item
    , subItems    = filter (/= item) (subItems task)
    , hintAsked   = False
    }

apply_decomposition (Hint h) task =
  task
    { taskMessages = taskMessages task ++ [h]
    , hintAsked    = False
    }


service_decomposition
  :: DecompositionTask
  -> DecompositionModel
  -> DecompositionTask
service_decomposition task model =
  scaffold contingency_decomposition apply_decomposition task model



-- | 2. Sao Pedro et al. -- Adaptive Task Scaffolding

-- | Sao Pedro, M., Baker, R., & Gobert, J. (2013).
-- | Incorporating scaffolding and tutor context into Bayesian knowledge
-- | tracing to predict inquiry skill acquisition.
-- |
-- | Provides increasingly specific support when an inquiry skill
-- | is not demonstrated.

type Skill = String

data InquiryTask = InquiryTask
  { inquirySkill      :: Skill
  , skillDemonstrated :: Bool
  , supportLevel      :: Int
  , supportEnabled    :: Bool
  , scaffoldLines     :: [(Int, String)]
  , agentMessages     :: [String]
  } deriving (Show, Eq)

data Opportunity = Opportunity
  { opportunitySkill  :: Skill
  , opportunityShown  :: Bool
  , opportunityHelped :: Bool
  } deriving (Show, Eq)

data InquiryModel = InquiryModel
  { masteryEstimates :: [(Skill, Double)]
  } deriving (Show)

data AgentTurn = AgentTurn
  { turnLevel :: Int
  , turnText  :: String
  } deriving (Show, Eq)


-- Updates the learner model using extended BKT.
update_extended_bkt :: Opportunity -> InquiryModel -> InquiryModel
update_extended_bkt = undefined


instance Model InquiryModel Opportunity where
  initModel =
    InquiryModel []

  update =
    update_extended_bkt


-- Available scaffolds: authored support at different levels.
instance AvailableScaffolds InquiryTask InquiryModel AgentTurn where
  available_scaffolds task _ =
    [ AgentTurn level text
    | (level, text) <- scaffoldLines task
    ]


-- Selects the next support level when the skill is not demonstrated.
contingency_inquiry
  :: InquiryTask
  -> InquiryModel
  -> [AgentTurn]
  -> Maybe AgentTurn
contingency_inquiry task _ available
  | not (supportEnabled task) =
      Nothing

  | skillDemonstrated task =
      Nothing

  | otherwise =
      first
        [ turn
        | turn <- available
        , turnLevel turn == supportLevel task + 1
        ]


-- Applies the selected scaffold.
apply_inquiry :: AgentTurn -> InquiryTask -> InquiryTask
apply_inquiry turn task =
  task
    { supportLevel  = turnLevel turn
    , agentMessages = agentMessages task ++ [turnText turn]
    }


service_inquiry
  :: InquiryTask
  -> InquiryModel
  -> InquiryTask
service_inquiry task model =
  scaffold contingency_inquiry apply_inquiry task model




-- | 3. Wambsganss et al. -- Dialog Scaffolding

-- | Wambsganss, T., Kueng, T., Soellner, M., & Leimeister, J. M. (2021).
-- | ArgueTutor: An adaptive dialog-based learning system for
-- | argumentation skills.
-- |
-- | Analyses the learner's writing and provides adaptive dialog support.

data ArgumentLabel
  = Claim
  | Premise
  | NonArgumentative
  deriving (Show, Eq)

data Segment = Segment
  { segmentText  :: String
  , segmentLabel :: ArgumentLabel
  } deriving (Show, Eq)

newtype Revision =
  Revision String
  deriving (Show, Eq)

data DialogModel = DialogModel
  { learnerSegments :: [Segment]
  } deriving (Show)

data DialogTask = DialogTask
  { learnerText      :: String
  , learnerUtterance :: String
  , dialogMessages   :: [String]
  } deriving (Show, Eq)

data Move
  = ArgumentFeedback String
  | Theory String
  | DialogResponse String
  deriving (Show, Eq)


-- Analyses argumentative text using a BERT-style classifier.
analyse_argument :: String -> [Segment]
analyse_argument = undefined


-- Produces adaptive feedback from the learner model.
argument_feedback :: DialogModel -> [Move]
argument_feedback = undefined


-- Produces a response using dialogue-intent classification.
dialog_response :: String -> Maybe Move
dialog_response = undefined


instance Model DialogModel Revision where
  initModel =
    DialogModel []

  update (Revision text) model =
    model { learnerSegments = analyse_argument text }


-- Available scaffolds: feedback and dialog guidance.
instance AvailableScaffolds DialogTask DialogModel Move where
  available_scaffolds task model =
       argument_feedback model
    ++ maybeToList (dialog_response (learnerUtterance task))


-- Selects the first available dialog support.
contingency_dialog
  :: DialogTask
  -> DialogModel
  -> [Move]
  -> Maybe Move
contingency_dialog _ _ =
  first


-- Adds the selected support to the dialog.
apply_dialog :: Move -> DialogTask -> DialogTask
apply_dialog move task =
  task
    { dialogMessages =
        dialogMessages task ++ [render move]
    }
  where
    render (ArgumentFeedback s) = s
    render (Theory s)           = s
    render (DialogResponse s)   = s


service_dialog
  :: DialogTask
  -> DialogModel
  -> DialogTask
service_dialog task model =
  scaffold contingency_dialog apply_dialog task model



-- | Helpers


first :: [a] -> Maybe a
first (x:_) = Just x
first []    = Nothing


maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []
