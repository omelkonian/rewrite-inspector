{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Basic datatypes.
-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, Rank2Types #-}

module Types where

import System.IO.Unsafe (unsafePerformIO)

import Data.Binary (decodeFile)
import Data.Char   (toLower)
import Text.Read   (readMaybe)
import Data.Text   (pack, unpack)
import qualified Data.Vector as V

import Lens.Micro    ((^.), (&), (.~), (%~), ix, Lens', lens)
import Lens.Micro.TH (makeLenses)

import Brick       ((<+>), str, txt, CursorLocation)
import Brick.Forms ((@@=), checkboxField, editField, formState, newForm, Form)

import Gen
import Precompute

-- * Basic types.

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

data UIState term = UIState
  { _curBinder :: Int
  -- ^ current binder
  , _curSteps  :: [Int]
  -- ^ current step for each binder

  -- * Options

  , _form      :: Form (OptionsUI term) NoCustomEvent Name
  -- ^ input form for setting parameters
  , _showBot   :: Bool
  -- ^ whether to hide bottom pane
  , _scroll    :: Bool
  -- ^ whether to scroll to focused region

  -- * Terminal attributes

  , _width     :: Int
  -- ^ current terminal width
  , _height    :: Int
  -- ^ current terminal height

  -- * Text search

  , _curOccur  :: Int
  -- ^ current occurrence we are highlighting
  , _leftN     :: Int
  -- ^ # of occurrences in the left viewport
  , _rightN    :: Int
  -- ^ # of occurrences in the right viewport
  }
makeLenses ''UIState

getCurrentStep
  :: forall term. Diff term
  => UIState term -> Int
getCurrentStep st = (st^.curSteps) !! (st^.curBinder)

-- | Create the input form.
mkForm
  :: forall term. Diff term
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

-- | Precomputed states.
precomputed :: Diff term => PrecomputedState (Ann term) (Ctx term)
precomputed = unsafePerformIO $ decodeFile "history'.dat"

states :: forall term. Diff term => StatesV (Ann term) (Ctx term)
states = fst (precomputed @term)

allBinders :: forall term. Diff term => [Binder]
allBinders = snd (precomputed @term)

getDiffScreen
  :: forall term. Diff term
  => UIState term
  -> DiffScreen (Ann term) (Ctx term)
getDiffScreen st
  = (!!!) @term (precomputed @term)
                (st^.curBinder, getCurrentStep st, st^.formData.opts, st^.width)

-- | Group the rewrite history by the different top-level binders.
initialState :: forall term. Diff term => UIState term
initialState = UIState
  { _curBinder = 0
  , _curSteps  = replicate (length $ allBinders @term) 0
  , _form      = mkForm @term (OptionsUI { _opts = initOptions @term
                                         , _com  = Step 1 })
  , _showBot   = False
  , _scroll    = True
  , _width     = 0
  , _height    = 0
  , _curOccur  = 0
  , _leftN     = 0
  , _rightN    = 0
  }

-- | Get the current string we are searching for.
-- NB: Returns the empty string of no search command has been issued.
getSearchString :: Diff term => UIState term -> String
getSearchString vs = case vs^.formData.com of { Search s -> s; _ -> "" }

-- | Lens into the input form's data.
formData
  :: forall term. Diff term
  => Lens' (UIState term) (OptionsUI term)
formData f vs = (\fm' -> vs {_form = mkForm @term fm'}) <$> f (formState $ _form vs)

-- | Get the available code width for one of the two code panes.
getCodeWidth :: UIState term -> Int
getCodeWidth vs = vs^.width `div` 2

maxBinder :: forall term. Diff term => Int
maxBinder = V.length (states @term) - 1

maxStep :: forall term. Diff term => UIState term -> Int
maxStep vs = V.length (states @term V.! (vs^.curBinder)) - 1

-- | Cycle through top-level binders.
--
--   [@stepBinder@] Proceed forward.
--
--   [@unstepBinder@] Proceed backward.
stepBinder, unstepBinder
  :: forall term. Diff term
  => UIState term -> UIState term
stepBinder vs = vs & curBinder %~ incr
  where incr i | i == maxBinder @term = 0
               | otherwise            = i + 1
unstepBinder vs = vs & curBinder %~ decr
  where decr i | i == 0    = maxBinder @term
               | otherwise = i - 1

-- | Cycle through transformations of the current binder.
--
--   [@step@] Proceed forward.
--
--   [@unstep@] Proceed backward.
--
--   [@reset@] Reset to the initial state.
step, unstep, reset
  :: forall term. Diff term
  => UIState term -> UIState term
step vs = vs & curSteps . ix (vs^.curBinder) %~ incr
  where incr n | n == maxStep vs = n
               | otherwise       = n + 1
unstep vs = vs & curSteps . ix (vs^.curBinder) %~ decr
  where decr n | n == 0    = n
               | otherwise = n - 1
reset vs = vs & curSteps . ix (vs^.curBinder) .~ 0

-- * User-issued commands.

-- | Some user commands have a notion of direction; either going forth or back.
data Direction = Forward | Backward

-- | Move to a specified step of the transformations of the current binder.
moveTo
  :: forall term. Diff term
  => Int -> UIState term -> UIState term
moveTo n vs = vs & curSteps . ix (vs^.curBinder) .~ n'
  where n' | n < 0          = 0
           | n > maxStep vs = maxStep vs
           | otherwise      = n

-- | Move to the next/previous step with the given transformation name.
nextTrans
  :: forall term. Diff term
  => Direction -> String
  -> UIState term -> UIState term
nextTrans dir (map toLower -> s) v0 = undefined

-- | Cycle through search occurrences.
nextOccur
  :: forall term. Diff term
  => Direction
  -> UIState term -> UIState term
nextOccur dir v = undefined
