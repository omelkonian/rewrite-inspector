{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Basic functionality for the terminal user-inteface (TUI).
-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module BrickUI where

import Prelude hiding (fail)

import System.Environment     (getArgs)
import Control.Applicative    ((<|>))
import Control.Monad          (void, (>>))
import Control.Monad.Fail     (MonadFail (..))
import Control.Monad.IO.Class (liftIO)

import Data.Binary (encodeFile)
import Data.Either (fromRight)
import Data.List   (sortOn)
import Data.Maybe  (listToMaybe, catMaybes)
import qualified Data.Vector as Vec

import Lens.Micro

import Brick
  ( App (..), BrickEvent (..), EventM, Next, Widget (..)
  , CursorLocation (..), cursorLocationName, cursorsL
  , VisibilityRequest (..), visibilityRequestsL
  , hSize, vSize
  , continue, halt
  , str, vBox, hBox
  )
import Brick.Focus  (focusRingCursor, focusGetCurrent)
import Brick.Themes (loadCustomizations, themeToAttrMap)
import qualified Brick                as B
import qualified Brick.Forms          as Bf
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty         as V

import Gen
import Precompute
import Types
import Pretty

-- | Entry point for the TUI.
runMain :: forall term. Diff term => FilePath -> IO ()
runMain ftheme = do
  args <- getArgs
  case args of
    ["-p", fname] -> do
      hist <- readHistory @term fname
      let hist' = precompute hist
      encodeFile "history'.dat" hist'
    ["-m"] -> do
      let dtheme = defaultTheme (userStyles @term)
      theme <- fromRight dtheme <$> loadCustomizations ftheme dtheme
      void $ B.defaultMain (app @term (themeToAttrMap theme)) initialState
    _ -> error " Usage: clash-term -p <history_file>, and then \
               \        clash-term -m"


-- | The 'Brick.App' configuration.
app :: Diff term => B.AttrMap -> App (UIState term) NoCustomEvent Name
app attrMap = App
  { appDraw         = drawUI
  , appChooseCursor = chooseCursor
  , appHandleEvent  = handleStart
  , appStartEvent   = (lookupSize <*>) . return
  , appAttrMap      = const attrMap
  }

-- | Choose a single cursor to display, out of possibly many requests.
chooseCursor :: UIState term -> [Cursor] -> Maybe Cursor
chooseCursor st ls
  =  (listToMaybe $ filter isSearch ls)
 <|> (focusRingCursor (Bf.formFocus . _form) st ls)
  where
    isSearch :: Cursor -> Bool
    isSearch = \case
      CursorLocation {cursorLocationName = Just (SearchResult _)} -> True
      _                                                           -> False

-- * Display.

-- Draw all top-level binders and their current step.
drawUI :: forall term. Diff term => UIState term -> [Widget Name]
drawUI vs =
  [ B.translateBy (B.Location controlsOffset) controls
  | vs^.showBot
  ]
  ++
  [ vBox
      [ B.vLimitPercent 20 $
          C.hCenter $
            hBoxSpaced 2 (drawBndr <$> zip [0..] (allBinders @term))
      , diff
      , B.vLimitPercent 10 $
          hBox
            [ inputForm
            , searchMatches
            ]
      ]
  ]
  where
    ds :: DiffScreen (Ann term) (Ctx term)
    ds = getDiffScreen vs

    -- * display the top-level binders
    drawBndr :: (Int, Binder) -> Widget Name
    drawBndr (i, bndr)
      = wb (  show (getCurrentStep vs)
           ++ "/" ++ show totN
           ++ " (" ++ (ds^.name') ++ ")"
           )
      $ str (fillSize 50 bndr)
      where
        totN = length (states @term Vec.! i)
        wb | i == vs^.curBinder = withBorderSelected
           | otherwise             = withBorder

    -- * display the diff of this rewrite step
    diff :: Widget Name
    diff =
      hBoxSpaced 2
        [ B.viewport LeftViewport B.Both $
            vL $ withBorder "Before" $ showE (ds^.before')
        , B.viewport RightViewport B.Both $
            vR $ withBorder "After" $ showE (ds^.after')
        ]
      where
        showE = showCode @term (vs^.scroll) (ds^.ctx') (getSearchString vs)
        curO  = vs^.curOccur
        ln    = vs^.leftN
        (vL, vR) | curO < ln = (visibleCursors curO, invisibleCursors)
                 | otherwise = (invisibleCursors, visibleCursors (curO - ln))

    -- * display the (editable) input form
    inputForm :: Widget Name
    inputForm = withBorder "Input"
              $ Bf.renderForm
              $ Bf.setFormConcat (hBoxSpaced 10)
              $ vs^.form

    searchMatches :: Widget Name
    searchMatches = C.vCenter $ str (n ++ " out of " ++ tot ++ " matches")
      where
        (n, tot) | let lr = vs^.leftN + vs^.rightN
                 , lr > 0
                 = (show (vs^.curOccur + 1), show lr)
                 | otherwise
                 = ("-", "-")

    -- * display the keyboard controls
    controlsOffset :: (Int, Int)
    controlsOffset = ( (vs^.width `div` 2) - 25
                     , (vs^.height `div` 2) - 15
                     )

    controls :: Widget Name
    controls = withBorder "Controls" $ vBoxSpaced
      [ "→ / ← (Ctrl-l / Ctrl-k)" .- "next/previous binder"
      , "↓ / ↑"                   .- "next/previous step"
      , "r"                       .- "reset"
      , "Escape"                  .- "quit"
      , "Shift-<dir>"             .- "scroll left pane"
      , "Ctrl-<dir>"              .- "scroll right pane"
      , "PageUp/Down"             .- "scroll both panes (up/down)"
      , "Home/End"                .- "(vertically) scroll to start/end"
      , "Ins/Del"                 .- "scroll both panes (left/right)"
      , "Ctrl-p"                  .- "show/hide keyboard controls"
      , "(Shift-)Tab"             .- "cycle through input fields"
      , "Enter"                   .- "submit move action (forward)"
      , "KBS/Ctrl-b"              .- "submit move action (backward)"
      , "Space"                   .- "toggle flag"
      ]
      where
        button .- desc = hBox [emph button, str $ " : " ++ desc]

-- * Event handling.

-- | Allow pattern matches in EventM monadic do blocks.
instance MonadFail (EventM Name) where
  fail = liftIO . fail

-- | Lookup terminal size and store in the current state.
lookupSize :: EventM Name (UIState term -> UIState term)
lookupSize = do
  out    <- V.outputIface <$> B.getVtyHandle
  (w, h) <- V.displayBounds out
  return $ (width .~ w) . (height .~ h)

-- | Update number of occurrences of searched string in both viewports.
updateOcc :: forall term. Diff term => UIState term -> UIState term
updateOcc vs
  | let ds = getDiffScreen @term vs
        ln = countOcc @term (getSearchString vs) (ds^.before')
        rn = countOcc @term (getSearchString vs) (ds^.after')
  , ln + rn > 0
  = vs & leftN    .~ ln
       & rightN   .~ rn
       & curOccur .~ ((vs^.curOccur) `mod` (ln + rn))

  | otherwise
  = vs

-- | Lookup code sizes and store them in the current state, then handle events.
handleStart :: forall term. Diff term
            => UIState term
            -> BrickEvent Name NoCustomEvent
            -> EventM Name (Next (UIState term))
handleStart vs ev = do
  pre <- lookupSize
  vs' <- handleEvent (pre vs) ev
  post <- lookupSize
  return (updateOcc . post <$> vs')

-- | Handle keyboard events.
handleEvent
  :: forall term. Diff term
  => UIState term
  -> BrickEvent Name NoCustomEvent
  -> EventM Name (Next (UIState term))
handleEvent vs ev@(VtyEvent (V.EvKey key mods))

  -- some controls are disabled when the user is writing in the input form
  | [] <- mods
  , focusGetCurrent (Bf.formFocus (vs^.form)) /= Just (FormField "Command")
  = sometimes

  -- the rest of the controls are active all the time
  | [] <- mods
  = always

  | [V.MShift] <- mods
  = shiftScroll

  | [V.MCtrl] <- mods
  = case key of
      -- show/hide bottom pane
      V.KChar 'p' -> continue (vs & showBot %~ not)
      -- action (forward)
      V.KChar 'b' -> action Backward
      -- change top-level binder
      V.KChar 'l' -> contT (stepBinder vs)
      V.KChar 'k' -> contT (unstepBinder vs)
      _        -> ctrlScroll

  | otherwise
  = continue vs

  where
    contT      = continue . (scroll .~ True)
    contF      = (>> continue (vs & scroll .~ False))
    bottom fg  = continue $ fg vs
                          & scroll .~ True
    action dir  = case vs^.formData.com of
      Step n   -> bottom $ moveTo n
      Trans s  -> bottom $ nextTrans dir s
      Search _ -> bottom $ nextOccur dir

    sometimes = case key of
      -- reset to initial step (of current binder)
      V.KChar 'r' -> bottom reset
      -- move to previous step/transformation
      V.KBS       -> action Backward
      -- change top-level binder
      V.KRight    -> contT (stepBinder vs)
      V.KLeft     -> contT (unstepBinder vs)
      _           -> always

    always = case key of
      -- basic controls
      V.KEsc      -> halt vs
      -- change step of current binder
      V.KDown     -> bottom step
      V.KUp       -> bottom unstep
      -- vertical scrolling (both)
      V.KPageDown -> contF (vScrollL     >> vScrollR)
      V.KPageUp   -> contF (vScrollL'    >> vScrollR')
      V.KHome     -> contF (vScrollHomeL >> vScrollHomeR)
      V.KEnd      -> contF (vScrollEndL  >> vScrollEndR)
      -- horizontal scrolling (both)
      V.KDel      -> contF (hScrollL  >> hScrollR)
      V.KIns      -> contF (hScrollL' >> hScrollR')
      -- move to next step/transformation
      V.KEnter    -> action Forward
      -- dispatch to form handler
      _           -> formHandler

    shiftScroll = contF $ case key of
      -- vertical/horizontal scrolling (left side)
      V.KDown  -> vScrollL
      V.KUp    -> vScrollL'
      V.KRight -> hScrollL
      V.KLeft  -> hScrollL'
      _        -> return ()

    ctrlScroll = contF $ case key of
      -- vertical/horizontal scrolling (right side)
      V.KDown  -> vScrollR
      V.KUp    -> vScrollR'
      V.KRight -> hScrollR
      V.KLeft  -> hScrollR'
      _        -> return ()

    -- form-handler
    formHandler = do
      fm' <- Bf.handleFormEvent ev (vs^.form)
      let cm          = (Bf.formState fm')^.com
          tot         = length $ states @term Vec.! (vs^.curBinder)
          valid       = case cm of Step n  -> n > 0 && n <= tot
                                   _       -> True
      continue $ vs & form .~ Bf.setFieldValid valid (FormField "Command") fm'

-- no-op event
handleEvent vs _ = continue vs

-- * Scrolling.

-- | The amount of scrolling with each request (in pixels).
scrollStep :: Int
scrollStep = 5

l, r :: B.ViewportScroll Name
l = B.viewportScroll LeftViewport
r = B.viewportScroll RightViewport

vScrollL, vScrollR, hScrollL, hScrollR, vScrollL', vScrollR', hScrollL', hScrollR',
  vScrollHomeL, vScrollHomeR, vScrollEndL, vScrollEndR :: EventM Name ()
vScrollL     = B.vScrollBy l scrollStep
vScrollL'    = B.vScrollBy l (-scrollStep)
vScrollR     = B.vScrollBy r scrollStep
vScrollR'    = B.vScrollBy r (-scrollStep)
hScrollL     = B.hScrollBy l scrollStep
hScrollL'    = B.hScrollBy l (-scrollStep)
hScrollR     = B.hScrollBy r scrollStep
hScrollR'    = B.hScrollBy r (-scrollStep)
vScrollHomeL = B.vScrollToBeginning l
vScrollHomeR = B.vScrollToBeginning r
vScrollEndL  = B.vScrollToEnd l
vScrollEndR  = B.vScrollToEnd r

-- | Gather all cursor placement requests coming from within the given 'Widget',
-- filter out only those that are the result of a /search/ command,
-- and convert the current one (based on the current occurrence number)
-- to a visibility request.
-- NB: Only to be used within a 'viewport'.
visibleCursors :: Int -> Widget Name -> Widget Name
visibleCursors n p = Widget (hSize p) (vSize p) $ do
  res <- B.render p
  let crs  = map fst
           $ sortOn ((\case {SearchResult i -> i; _ -> 0}) . snd)
           $ catMaybes
           $ map (\c ->  case cursorLocationName c of
              Just s@(SearchResult _) -> Just (c, s)
              _                       -> Nothing)
           $ (res^.cursorsL)
  if null crs then
    return res
  else do
    let c = crs !! (n `mod` length crs)
    return $ res & visibilityRequestsL .~ [VR { vrPosition = cursorLocation c
                                              , vrSize     = (1, 1)
                                              }]
                 & cursorsL .~ [c]

-- | Remove all cursor placement requests coming from within the given 'Widget'.
-- NB: Only to be used within a 'viewport'.
invisibleCursors :: Widget n -> Widget n
invisibleCursors p = Widget (hSize p) (vSize p) $ do
  res <- B.render p
  return $ res & cursorsL .~ []
