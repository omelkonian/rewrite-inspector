{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Basic functionality for the terminal UI.
-}
{-# LANGUAGE OverloadedStrings #-}

module BrickUI (runTerminal) where

import System.Environment (getArgs)
import Control.Monad      (void, (>>))
import Lens.Micro

import Brick
  ( App (..), BrickEvent (..), EventM, Next, Widget
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
import Types
import Pretty

-- | Entry point.
runTerminal :: forall term. Diff term => FilePath -> IO ()
runTerminal ftheme = do
  res <- loadCustomizations ftheme (defaultTheme $ userStyles @term)
  case res of
    Left err ->
      error $ "[theme.ini] Configuration file is malformed: " ++ err
    Right theme -> do
      [fname] <- getArgs
      hist    <- readHistory @term fname
      void $ B.customMain
        -- Vty configuration
        (V.mkVty $ V.defaultConfig { V.vtime = Just 100, V.vmin  = Just 1 })
        Nothing                             -- event channel
        (app @term (themeToAttrMap theme))  -- the Brick application
        (createVizStates @term hist)        -- initial state

-- | The Brick App.
app :: forall term. Diff term
    => B.AttrMap -> App (VizStates term) NoCustomEvent Name
app attrMap = App { appDraw         = drawUI
                  , appChooseCursor = focusRingCursor (Bf.formFocus . _form)
                  , appHandleEvent  = handleStart
                  , appStartEvent   = (lookupSize <*>) . return
                  , appAttrMap      = const attrMap
                  }

-- | Top-level: Draw all top-level binders and their current step.
-- NB: delegates bottom-level to `drawUI`
drawUI :: forall term. Diff term
       => VizStates term -> [Widget Name]
drawUI vs =
  [ B.translateBy (B.Location controlsOffset) controls
  | vs^.showBot
  ]
  ++
  [ vBox
      [ B.vLimitPercent 20 $
          C.hCenter $
            hBoxSpaced 2 (drawBndr <$> vs^.binders)
      , diff
      , B.vLimitPercent 10 inputForm
      ]
  ]
  where
    -- display the top-level binders
    drawBndr :: Binder -> Widget Name
    drawBndr bndr
      = wb (show curN ++ "/" ++ show totN ++ " (" ++ stepName ++ ")")
      $ str (fillSize 50 bndr)
      where
        (curN, totN, stepName) = getStep vs bndr
        wb | bndr == vs^.curBinder = withBorderSelected
           | otherwise             = withBorder

    -- display the diff of this rewrite step
    diff :: Widget Name
    diff
      | v@(VizState (st:_) _ curE _) <- getCurrentState vs
      = let
          showE = showCode (vs^.scroll)
                           (min 80 $ getCodeWidth vs)
                           (vs^.formData^.opts)
                           (case vs^.formData.trans of {Search s -> s; _ -> ""})
                           (st^.ctx)
          nextE = step v ^. curExpr
        in
          hBoxSpaced 2
            [ B.viewport LeftViewport B.Both $
                withBorder "Before" $ showE curE
            , B.viewport RightViewport B.Both $
                withBorder "After" $ showE nextE
            ]

      | otherwise
      = C.center $ title "THE END"

    -- display the (editable) input form
    inputForm :: Widget Name
    inputForm = withBorder "Input"
              $ Bf.renderForm
              $ Bf.setFormConcat (hBoxSpaced 10)
              $ vs^.form

    -- display the keyboard controls
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
      , "Enter"                   .- "submit move action"
      , "Space"                   .- "toggle flag"
      ]
      where
        button .- desc = hBox [emph button, str $ " : " ++ desc]

-- Lookup terminal size and store in the current state.
lookupSize :: EventM Name (VizStates term -> VizStates term)
lookupSize = do
  out    <- V.outputIface <$> B.getVtyHandle
  (w, h) <- V.displayBounds out
  return $ (width .~ w) . (height .~ h)

-- Lookup code sizes and store them in the current state, then handle events.
handleStart :: forall term. Diff term
            => VizStates term
            -> BrickEvent Name NoCustomEvent
            -> EventM Name (Next (VizStates term))
handleStart vs ev = do
  pre <- lookupSize
  vs' <- handleEvent (pre vs) ev
  post <- lookupSize
  return (post <$> vs')

-- | Handle keyboard events.
handleEvent :: Diff term
            => VizStates term
            -> BrickEvent Name NoCustomEvent
            -> EventM Name (Next (VizStates term))
handleEvent vs ev@(VtyEvent (V.EvKey key mods))

  -- some controls are disabled when the user is writing in the input form
  | [] <- mods
  , focusGetCurrent (Bf.formFocus (vs^.form)) /= Just (FormField "Trans")
  = sometimes

  -- the rest of the controls are active all the time
  | [] <- mods
  = always

  | [V.MShift] <- mods
  = case key of
      -- search (next)
      V.KEnter -> search unstep
      _        -> shiftScroll

  | [V.MCtrl] <- mods
  = case key of
      -- show/hide bottom pane
      V.KChar 'p' -> continue (vs & showBot %~ not)
      -- search (previous)
      V.KEnter    -> search unstep
      -- change top-level binder
      V.KChar 'l' -> contT (stepBinder vs)
      V.KChar 'k' -> contT (unstepBinder vs)
      _        -> ctrlScroll

  | otherwise
  = continue vs

  where
    contT      = continue . (scroll .~ True)
    contF      = (>> continue (vs & scroll .~ False))
    bottom fg  = continue $ updateState vs (fg $ getCurrentState vs)
                          & scroll .~ True
    search f  = case vs^.formData.trans of
      Step n   -> bottom $ moveTo n
      Name s   -> bottom $ nextTrans f s
      _        -> continue vs

    sometimes = case key of
      -- reset to initial step (of current binder)
      V.KChar 'r' -> bottom reset
      -- move to previous step/transformation
      V.KBS       -> search unstep
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
      V.KEnter    -> search step
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
      let tr          = (Bf.formState fm')^.trans
          (_, tot, _) = getStep vs (vs^.curBinder)
          valid       = case tr of Step n  -> n > 0 && n <= tot
                                   _       -> True
      continue $ vs & form .~ Bf.setFieldValid valid (FormField "Trans") fm'

-- no-op event
handleEvent vs _ = continue vs

-- Scrolling, viewports.
l, r :: B.ViewportScroll Name
l = B.viewportScroll LeftViewport
r = B.viewportScroll RightViewport

scrollStep :: Int
scrollStep = 5

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
