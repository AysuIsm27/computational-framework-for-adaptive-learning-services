{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module ScaffoldImplementations where

import Scaffold
import Data.Maybe (fromMaybe)

-- | -----------------------------------------------------------------------
-- | Scaffold Implementations
-- | -----------------------------------------------------------------------
-- | Three scaffold services. Each gives the support a system has available
-- | for a task state, a contingency function saying which of it is due, and
-- | an apply function that puts it into the task. The service is the generic
-- | scaffold_task applied to the last two.
-- |
-- | The undefined functions are the ones a system supplies: trained models,
-- | authored content, and rules that are design decisions. Filling them in is
-- | enough to run any of the three.
-- | -----------------------------------------------------------------------

type Identifier = String


-- | ==========================================================
-- | 1. Task decomposition
-- | ==========================================================
-- | An item comes with a sequence of sub-items, one per solution step. Answer
-- | the item wrongly and you are moved into that sequence, a sub-item at a
-- | time. Hints come only when asked for, and an error message when the
-- | answer matches one that was authored.

data Item = Item
  { itemId    :: Int
  , itemText  :: String
  , itemHints :: [String]              -- last one gives the answer
  , itemBugs  :: [(String, String)]    -- wrong answer, message
  } deriving (Show, Eq)

data Answer = Answer
  { answeredItem :: Int
  , answerText   :: String
  , correct      :: Bool
  } deriving (Show, Eq)

data Decomposable = Decomposable
  { wholeItem :: Item
  , working   :: Item
  , answers   :: [Answer]        -- newest first
  , asked     :: Int             -- hints asked for on the working item
  , messages  :: [String]
  } deriving (Show, Eq)

newtype DecompositionModel = DecompositionModel
  { subItems :: [(Int, [Item])] } deriving (Show)

data Step = SubItem Item | Hint String | BugMessage String deriving (Show, Eq)

instance Model DecompositionModel Answer where
  initial    = DecompositionModel []
  update _ m = m

answerTo :: Decomposable -> Item -> Maybe Answer
answerTo task i = case filter ((== itemId i) . answeredItem) (answers task) of
  (a:_) -> Just a
  []    -> Nothing

solved :: Decomposable -> Item -> Bool
solved task i = maybe False correct (answerTo task i)

failed :: Decomposable -> Item -> Bool
failed task i = maybe False (not . correct) (answerTo task i)

instance AvailableScaffolds Decomposable DecompositionModel Step where
  available_scaffolds task model =
       [ SubItem i    | i <- fromMaybe [] (lookup (itemId (wholeItem task)) (subItems model)) ]
    ++ [ Hint h       | h <- itemHints (working task) ]
    ++ [ BugMessage m | (_, m) <- itemBugs (working task) ]

contingency_decomposition :: Decomposable -> DecompositionModel -> [Step] -> Maybe Step
contingency_decomposition task _ steps
  | not (failed task (wholeItem task)) = Nothing
  | otherwise = first (nextSubItem ++ bugMessage ++ nextHint)
  where
    nextSubItem = take 1 [ SubItem i | SubItem i <- steps, not (solved task i) ]
    nextHint    = take 1 (drop (asked task - 1) [ h | h@(Hint _) <- steps ])
    bugMessage  = case answerTo task (working task) of
      Just a | not (correct a) ->
        [ BugMessage m | (wrong, m) <- itemBugs (working task), wrong == answerText a ]
      _ -> []
    first (x:_) = Just x
    first []    = Nothing

apply_decomposition :: Step -> Decomposable -> Decomposable
apply_decomposition (SubItem i) task    = task { working  = i }
apply_decomposition (Hint h) task       = task { messages = messages task ++ [h] }
apply_decomposition (BugMessage m) task = task { messages = messages task ++ [m] }

service_decomposition :: Decomposable -> DecompositionModel -> Decomposable
service_decomposition = scaffold_task contingency_decomposition apply_decomposition


-- | ==========================================================
-- | 2. Agent support in an open task
-- | ==========================================================
-- | The learner works in an open environment. While they are producing, a
-- | classifier over the action log says whether a process skill is being
-- | shown; when it is not, an agent speaks. Each further turn on the same
-- | skill is more specific than the last, up to the final one authored.
-- | Later activities run with the support switched off.

type Skill = Identifier

data Stage = Preparation | Production | Interpretation deriving (Show, Eq)

data Statement = Statement { manipulated :: String, observed :: String } deriving (Show, Eq)

newtype Act = Act { actSettings :: [(String, Double)] } deriving (Show, Eq)

data OpenTask = OpenTask
  { stage     :: Stage
  , statement :: Maybe Statement
  , acts      :: [Act]
  , turns     :: [String]
  , given     :: [(Skill, Int)]   -- turns already spent per skill
  , supportOn :: Bool
  } deriving (Show, Eq)

data Verdict = Shown | NotShown | Unclear deriving (Show, Eq)

-- supplied by the system: the trained classifier, the number of actions it
-- needs, and the rule used below that
classify :: Skill -> Maybe Statement -> [Act] -> Verdict
classify = undefined

needsActs :: Skill -> Int
needsActs = undefined

shortRule :: Skill -> Maybe Statement -> [Act] -> Verdict
shortRule = undefined

verdict :: Skill -> OpenTask -> Verdict
verdict skill task
  | length (acts task) >= needsActs skill = classify  skill (statement task) (acts task)
  | otherwise                             = shortRule skill (statement task) (acts task)

-- authored: what the agent says, per skill and level, with placeholders
data Line = Line { lineSkill :: Skill, lineLevel :: Int, lineText :: String } deriving (Show, Eq)

fillIn :: Maybe Statement -> String -> String
fillIn Nothing  s = s
fillIn (Just t) s = swap "[MANIPULATED]" (manipulated t) (swap "[OBSERVED]" (observed t) s)

swap :: String -> String -> String -> String
swap from to s@(c:cs)
  | take (length from) s == from = to ++ swap from to (drop (length from) s)
  | otherwise                    = c : swap from to cs
swap _ _ [] = []

data AgentTurn = AgentTurn Skill Int String deriving (Show, Eq)

data Tracing = Tracing
  { pInit    :: Double
  , pGuess   :: Double
  , pSlip    :: Double
  , tAlone   :: Double
  , tHelped  :: Double
  } deriving (Show, Eq)

data OpenTaskModel = OpenTaskModel
  { lines_     :: [Line]
  , tracing    :: [(Skill, Tracing)]
  , knowledge  :: [(Skill, Double)]
  } deriving (Show)

-- authored: fitted parameters, one set per skill
fittedTracing :: [(Skill, Tracing)]
fittedTracing = undefined

data Opportunity = Opportunity { oppSkill :: Skill, oppVerdict :: Verdict, oppHelped :: Bool }
  deriving (Show, Eq)

pKnown :: OpenTaskModel -> Skill -> Double
pKnown model skill = fromMaybe start (lookup skill (knowledge model))
  where start = maybe 0 pInit (lookup skill (tracing model))

instance Model OpenTaskModel Opportunity where
  initial = OpenTaskModel [] fittedTracing [ (s, pInit p) | (s, p) <- fittedTracing ]
  update opp model = case lookup (oppSkill opp) (tracing model) of
    Nothing -> model
    Just p  -> model { knowledge = (oppSkill opp, next) : rest }
      where
        rest  = filter ((/= oppSkill opp) . fst) (knowledge model)
        prior = pKnown model (oppSkill opp)
        -- being helped counts as not having shown the skill
        shown = not (oppHelped opp) && oppVerdict opp == Shown
        post  | shown     = prior * (1 - pSlip p)
                            / (prior * (1 - pSlip p) + (1 - prior) * pGuess p)
              | otherwise = prior * pSlip p
                            / (prior * pSlip p + (1 - prior) * (1 - pGuess p))
        learn = if oppHelped opp then tHelped p else tAlone p
        next  = post + (1 - post) * learn

instance AvailableScaffolds OpenTask OpenTaskModel AgentTurn where
  available_scaffolds task model =
    [ AgentTurn (lineSkill l) (lineLevel l) (fillIn (statement task) (lineText l))
    | l <- lines_ model ]

contingency_agent :: OpenTask -> OpenTaskModel -> [AgentTurn] -> Maybe AgentTurn
contingency_agent task _ available
  | not (supportOn task)     = Nothing
  | stage task /= Production = Nothing
  | otherwise = case due of
      (t:_) -> Just t
      []    -> Nothing
  where
    due = [ t | t@(AgentTurn skill level _) <- available
              , verdict skill task == NotShown
              , level == min (lastLevel skill) (1 + spent skill) ]
    spent skill     = fromMaybe 0 (lookup skill (given task))
    lastLevel skill = maximum (1 : [ l | AgentTurn s l _ <- available, s == skill ])

apply_agent :: AgentTurn -> OpenTask -> OpenTask
apply_agent (AgentTurn skill _ text) task = task
  { turns = turns task ++ [text]
  , given = (skill, 1 + fromMaybe 0 (lookup skill (given task)))
            : filter ((/= skill) . fst) (given task) }

service_agent :: OpenTask -> OpenTaskModel -> OpenTask
service_agent = scaffold_task contingency_agent apply_agent


-- | ==========================================================
-- | 3. Dialog tutoring over a written text
-- | ==========================================================
-- | The learner writes inside a dialog. A trained model labels the segments
-- | of the text, authored intents fire against that labelling, and the agent
-- | takes a turn. When nothing fires the dialog stops and the learner writes
-- | on their own.

data Segment = Segment { segmentText :: String, segmentLabel :: Identifier } deriving (Show, Eq)

data Intent = Intent { intentName :: Identifier, intentText :: String } deriving (Show, Eq)

data DialogModel = DialogModel
  { intents :: [Intent]
  , theory  :: [(Identifier, String)]
  } deriving (Show)

data Move = Explain String | Theory Identifier String | Respond Intent | Score Double
  deriving (Show, Eq)

data WritingTask = WritingTask
  { assignment :: String
  , text       :: String
  , dialog     :: [String]
  } deriving (Show, Eq)

newtype Revision = Revision String deriving (Show, Eq)

instance Model DialogModel Revision where
  initial    = DialogModel [] []
  update _ m = m

-- supplied by the system: the labelling model, the score, which intents fire,
-- and where explanation and theory belong in the flow
label :: DialogModel -> String -> [Segment]
label = undefined

score :: String -> Double
score = undefined

firing :: DialogModel -> [Segment] -> [Intent]
firing = undefined

guidance :: DialogModel -> WritingTask -> [Move] -> Maybe Move
guidance = undefined

instance AvailableScaffolds WritingTask DialogModel Move where
  available_scaffolds task model =
       [ Explain (assignment task) ]
    ++ [ Theory name t | (name, t) <- theory model ]
    ++ [ Respond i | i <- intents model ]
    ++ [ Score (score (text task)) ]

contingency_dialog :: WritingTask -> DialogModel -> [Move] -> Maybe Move
contingency_dialog task model available = case guidance model task available of
  Just move -> Just move
  Nothing   -> case [ Respond i | Respond i <- available, i `elem` fired ] of
    (move:_) -> Just move
    []       -> Nothing
  where fired = firing model (label model (text task))

apply_dialog :: Move -> WritingTask -> WritingTask
apply_dialog move task = task { dialog = dialog task ++ [say move] }
  where
    say (Explain t)  = t
    say (Theory _ t) = t
    say (Respond i)  = intentText i
    say (Score s)    = "Score: " ++ show s

service_dialog :: WritingTask -> DialogModel -> WritingTask
service_dialog = scaffold_task contingency_dialog apply_dialog
