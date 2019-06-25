{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Basic datatypes.
-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, Rank2Types #-}

module Types where

import Data.Char  (toLower)
import Data.List  (delete, elemIndex, find, isInfixOf, groupBy, nub, sortOn)
import Data.Maybe (fromJust)
import Data.Map   ((!), Map, insert, fromList)
import Text.Read  (readMaybe)
import Data.Text  (pack, unpack)

import Lens.Micro    ((^.), (&), (.~), (%~), Lens', lens)
import Lens.Micro.TH (makeLenses)

import Brick       ((<+>), str, txt, CursorLocation)
import Brick.Forms ((@@=), checkboxField, editField, formState, newForm, Form)

import Gen

-- * Basic types.

-- | Program binders (i.e. top-level identifiers) are identified by their name.
type Binder = String

-- | Our @Brick@ TUI does not use any custom events.
data NoCustomEvent

-- | The type of resource names, used throughout the TUI.
data Name
  = LeftViewport | RightViewport
  -- ^ viewports
  | FormField String
  -- ^ form fields
  | SearchResult Int
  -- ^ search results with numbered occurrences
  | Other
  deriving (Eq, Ord, Show)

-- | Type of cursors in our TUI.
type Cursor = CursorLocation Name

-- | Commands that the user can issue through the input form.
data Command
  = Step Int
  -- ^ move to given step in the current binder
  | Trans String
  -- ^ move to the next/previous transformation with the given name
  | Search String
  -- ^ move to the next/previous occurrence of the searched string
  deriving (Eq, Ord, Show)

-- | Options kept and changed throught the TUI's input form.
data OptionsUI term = OptionsUI
  { _opts :: Options term
  -- ^ options for the pretty-printer
  , _com  :: Command
  -- ^ command to issue (search string, etc...)
  }
makeLenses ''OptionsUI

-- | Bottom-level state of the UI (navigate steps of a top-level binder).
data VizState term = VizState
  { _steps     :: History term (Ctx term)
  -- ^ steps of the rewriting process
  , _prevState :: Maybe (VizState term)
  -- ^ previous state (initially Nothing)
  , _curExpr   :: term
  -- ^ current (intermediate) expression
  , _curStep   :: Int
  -- ^ current step in given top-level entity
  , _curOccur  :: Int
  -- ^ current occurrence we are highlighting
  , _leftN     :: Int
  -- ^ # of occurrences in the left viewport
  , _rightN    :: Int
  -- ^ # of occurrences in the right viewport
  }
makeLenses ''VizState

-- | Top-level state of the UI (navigate top-level binders).
data VizStates term = VizStates
  { _binders   :: [Binder]
  -- ^ all top-level binders
  , _curBinder :: Binder
  -- ^ currently selected binder
  , _states    :: Map Binder (VizState term)
  -- ^ state of each binder
  , _form      :: Form (OptionsUI term) NoCustomEvent Name
  -- ^ input form for setting parameters
  , _showBot   :: Bool
  -- ^ whether to hide bottom pane
  , _width     :: Int
  -- ^ current terminal width
  , _height    :: Int
  -- ^ current terminal height
  , _scroll    :: Bool
  -- ^ whether to scroll to focused region
  }
makeLenses ''VizStates

-- * Getters and setters.

-- | Create the input form.
mkForm :: forall term. Diff term
       => OptionsUI term
       -> Form (OptionsUI term) NoCustomEvent Name
mkForm = newForm $
  map (\(f, g, s) -> checkboxField (opts . lens f g) (FormField s) (pack s))
      (flagFields @term)
  ++
  [ (str "move to " <+>) @@=
      editField com -- lens
                (FormField "Command") -- resource name
                (Just 1) -- line limit
                (pack . concat . tail . words . show) -- display
                (readCom . unpack . head) -- validate
                (txt . head) -- render
                id -- rendering augmentation
  ]
  where
    readCom x | Just n <- readMaybe x :: Maybe Int = Just $ Step n
              | '%':'s':' ':s <- x                 = Just $ Search s
              | '%':'t':' ':s <- x                 = Just $ Trans s
              | otherwise                          = Nothing

-- | Group the rewrite history by the different top-level binders.
createVizStates :: forall term. Diff term
                => History term (Ctx term)
                -> VizStates term
createVizStates hist = VizStates
  { _binders    = top : (delete top $ nub bndrs)
  , _curBinder  = top
  , _states     = fromList
                $ map (\h -> (head h ^. bndrS, initialState h))
                $ groupBy (\ x y -> x^.bndrS == y^.bndrS)
                $ sortOn _bndrS hist
  , _form       = mkForm @term (OptionsUI { _opts = initOptions @term
                                          , _com  = Step 1 })
  , _showBot    = False
  , _width      = 0
  , _height     = 0
  , _scroll     = True
  }
  where bndrs = _bndrS <$> hist
        top   = fromJust $ find (topEntity @term `isInfixOf`) bndrs

-- | State initialization for the bottom layer.
initialState :: Diff term => History term (Ctx term) -> VizState term
initialState hist = VizState { _steps      = hist
                             , _prevState  = Nothing
                             , _curExpr    = initialExpr hist
                             , _curStep    = 1
                             , _curOccur   = 0
                             , _leftN      = 0
                             , _rightN     = 0 }

-- | Get the name of the current transformation.
currentStepName :: VizState term -> String
currentStepName v =
  case v^.steps of
    []     -> "THE END"
    (st:_) -> st^.name

-- | Get information about the current step.
getStep :: VizStates term
        -> Binder
        -> (Int {-current-}, Int {-total-}, String {-transformation-})
getStep vs bndr = (cur, cur + length (v^.steps), currentStepName v)
  where v   = (vs^.states) ! bndr
        cur = v^.curStep

-- | Get the current state of the bottom layer.
getCurrentState :: VizStates term -> VizState term
getCurrentState vs = (vs^.states) ! (vs^.curBinder)

-- | Get the current string we are searching for.
-- NB: Returns the empty string of no search command has been issued.
getSearchString :: Diff term => VizStates term -> String
getSearchString vs = case vs^.formData.com of { Search s -> s; _ -> "" }

-- | Lens from the /global/ state to the input form's data.
formData :: forall term
          . Diff term
         => Lens' (VizStates term) (OptionsUI term)
formData f vs = (\fm' -> vs {_form = mkForm @term fm'}) <$> f (formState $ _form vs)

-- | Update the /local/ state of the current binder.
updateState :: VizStates term -> VizState term -> VizStates term
updateState vs v = vs & states .~ insert (vs^.curBinder) v (vs^.states)

-- | Get the available code width for one of the two code panes.
getCodeWidth :: VizStates term -> Int
getCodeWidth vs = vs^.width `div` 2

-- | Cycle through top-level binders in the /global/ state.
--
--   [@stepBinder@] Proceed forward.
--
--   [@unstepBinder@] Proceed backward.
stepBinder, unstepBinder :: VizStates term -> VizStates term
stepBinder vs = vs & curBinder .~ findNext (vs^.curBinder) (vs^.binders)
  where findNext :: Eq a => a -> [a] -> a
        findNext x xs
          | Just i <- x `elemIndex` xs, i < (length xs - 1) = xs !! (i + 1)
          | otherwise                                       = x
unstepBinder vs = vs & curBinder .~ findPrev (vs^.curBinder) (vs^.binders)
  where findPrev :: Eq a => a -> [a] -> a
        findPrev x xs
          | Just i <- x `elemIndex` xs, i > 0 = xs !! (i - 1)
          | otherwise                         = x

-- | Cycle through transformations of the current binder in the /local/ state.
--
--   [@step@] Proceed forward.
--
--   [@unstep@] Proceed backward.
--
--   [@reset@] Reset to the initial state.
step, unstep, reset :: Diff term => VizState term -> VizState term
step prev@(VizState []     _ _    _    _ _ _) = prev
step prev@(VizState (t:ts) _ curE curS _ _ _) =
  VizState { _steps     = ts
           , _prevState = Just prev
           , _curExpr   = patch curE (t^.ctx) (t^.after)
           , _curStep   = curS + 1
           , _curOccur  = 0
           , _leftN     = 0
           , _rightN    = 0 }
unstep st = case st^.prevState of
  Nothing   -> st
  Just prev -> prev
reset first@(VizState _ Nothing _ _ _ _ _) = first
reset (VizState _ (Just prev) _ _ _ _ _)   = reset prev

-- * User-issued commands.

-- | Some user commands have a notion of direction; either going forth or back.
data Direction = Forward | Backward

-- | Move to a specified step of the transformations of the current binder.
moveTo :: Diff term => Int -> VizState term -> VizState term
moveTo n v = if (v^.curStep) == (v'^.curStep) then v else moveTo n v'
  where v' = case n `compare` (v^.curStep) of { EQ -> v
                                              ; LT -> unstep v
                                              ; GT -> step v }

-- | Move to the next/previous step with the given transformation name.
nextTrans :: Diff term => Direction -> String -> VizState term -> VizState term
nextTrans dir (map toLower -> s) v0 = go (next v0)
  where
    next = case dir of { Forward -> step ; Backward -> unstep }
    startStep = v0^.curStep
    go v -- not found, abort
         | v^.curStep == startStep = v
         -- end of steps, reset
         | [] <- v^.steps = go (reset v)
         -- found, return
         | (st:_) <- v^.steps
         , s `isInfixOf` (toLower <$> st^.name) = v
         -- continue searching..
         | otherwise = go (next v)

-- | Cycle through search occurrences.
nextOccur :: Diff term => Direction -> VizState term -> VizState term
nextOccur Forward  v = v & curOccur %~ (+ 1)
nextOccur Backward v = v & curOccur %~ back
  where back 0 = max 0 (v^.leftN + v^.rightN - 1)
        back i = i - 1
