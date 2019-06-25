{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Pretty-printing utilities and styling for the UI.
-}

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Pretty where

import Control.Arrow (first, second)

import Data.List                 (nub, isPrefixOf, findIndices, sortOn)
import Data.Text                 (unpack)
import Data.Text.Prettyprint.Doc ( layoutPretty, layoutCompact
                                 , LayoutOptions (..)
                                 , PageWidth (..), SimpleDocStream (..) )

import Brick (Widget, str, hBox, vBox)
import qualified Brick                      as B
import qualified Brick.Forms                as Bf
import qualified Brick.Themes               as Bt
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.Border       as Br
import qualified Brick.Widgets.Border.Style as BrS
import qualified Graphics.Vty               as V


import Gen
import Types

----------------------------------------------------------------------------
-- Pretty-printing Cλash Core.

countOcc
  :: forall term
   . Diff term
  => Options term  -- ^ options for Clash's pretty-printer
  -> String        -- ^ the string to search for
  -> term          -- ^ code to search in
  -> Int           -- ^ total number of occurrences
countOcc opts searchString
  = foldl (\cur d -> cur + countDoc d) 0
  . myForm @term [] [] [] []
  . layoutCompact
  . ppr' @term opts
  where
    countDoc :: MyDoc -> Int
    countDoc = \case
      MString s -> snd (highlightSearch undefined 0 s searchString)
      MMod _ d  -> countDoc d
      MLine _   -> 0

showCode
  :: forall n term
   . Diff term
  => Name          -- ^ viewport name
  -> Bool          -- ^ whether to scroll to focused region
  -> Int           -- ^ maximum line width
  -> Options term  -- ^ options for Clash's pretty-printer
  -> [Ctx term]    -- ^ the current context
  -> String        -- ^ the string to search for
  -> term          -- ^ code to display
  -> Widget Name   -- ^ the Brick widget to display
showCode vn scroll w opts ctx0 searchString
  = vBox
  . fmap (render vn scroll searchString 0) . split . (MLine 0 :)
  . myForm @term ctx0 [] [] []
  . layoutPretty (LayoutOptions (AvailablePerLine w 0.8))
  . ppr' @term opts

-- Convert a stream into our simpler data type.
data MyDoc = MString String
           | MLine Int
           | MMod [String] MyDoc

data Item = Stx | Ctx

myForm :: forall term. Diff term
       => [Ctx term] -> [Ctx term] -> [Item] -> [String]
       -> SimpleDocStream (Ann term) -> [MyDoc]
myForm ctx0 ctx' stack attrs = \case
  SFail           -> error "split.SFail"
  SEmpty          -> [mark (MString "")]
  SChar c rest    -> mark (MString [c])        : continueWith rest
  SText _ s rest  -> mark (MString (unpack s)) : continueWith rest
  SLine i rest    -> MLine i                   : continueWith rest
  SAnnPush a rest -> case handleAnn @term a of
    Left  s -> myForm @term ctx0 ctx' (Stx:stack) (show s:attrs) rest
    Right c -> myForm @term ctx0 (ctx' ++ [c]) (Ctx:stack) attrs rest
  SAnnPop rest -> case top of
    Stx -> myForm @term ctx0 ctx' stack' (tail attrs) rest
    Ctx -> myForm @term ctx0 (init ctx') stack' attrs rest
    where (top:stack') = stack
  where continueWith = myForm @term ctx0 ctx' stack attrs
        mark = MMod $ ["focus" | ctx0 `isPrefixOf` ctx'] ++ attrs

-- Split into individual lines (of some indendation).
split :: [MyDoc] -> [(Int, [MyDoc])]
split []             = []
split (MLine i : ys) = (i, ysL) : split ysR
  where (ysL, ysR) = break (\case (MLine _) -> True ; _ -> False) ys
split _              = error "split: does not start with STLine"

-- Render a single line in Brick (highlighting when marked).
render :: Name -> Bool -> String -> Int -> (Int, [MyDoc]) -> Widget Name
render vn scroll searchString n0 (i, xs) = B.padLeft (B.Pad i)
                                   $ hBox
                                   $ render1 n0 xs
  where
    render1 :: Int -> [MyDoc] -> [Widget Name]
    render1 _ []       = []
    render1 n (d : ds) = w : render1 n' ds
      where (w, n') = f n d

    f :: Int -> MyDoc -> (Widget Name, Int)
    f n = \case
      MMod attrs w -> first (modify scroll (nub attrs)) (f n w)
      MString s    -> highlightSearch vn n s searchString
      _            -> error "render1"



----------------------------------------------------------------------------
-- UI Styling.

defaultTheme :: [(String, V.Attr)] -> Bt.Theme
defaultTheme extraAttrs = Bt.newTheme V.defAttr $
  [ ("focus",      B.bg $ V.rgbColor 47 79 79)
  , ("title",      V.defAttr `V.withStyle` V.bold)
  , ("emph",       V.defAttr `V.withStyle` V.bold)
  , ("search",     V.defAttr `V.withStyle` V.underline)
    -- forms
  , (E.editAttr,              V.white `B.on` V.black)
  , (E.editFocusedAttr,       V.black `B.on` V.yellow)
  , (Bf.invalidFormInputAttr, V.white `B.on` V.red)
  , (Bf.focusedFormInputAttr, V.black `B.on` V.yellow)
    -- syntax highlighting
  , ("type",      V.defAttr `V.withForeColor` V.brightYellow)
  , ("keyword",   V.defAttr `V.withForeColor` V.rgbColor 255 165 0)
  , ("literal",   V.defAttr `V.withForeColor` V.brightCyan)
  , ("unique",    V.defAttr `V.withStyle` V.dim)
  , ("qualifier", V.defAttr `V.withStyle` V.italic)
  ] ++ map (\(s, a) -> (B.attrName s, a)) extraAttrs

modify :: Bool -> [String] -> Widget n -> Widget n
modify scroll = foldr (.) id . fmap mod1 . sortOn (\case "Type" -> 1; _ -> 0)
  where
    mod1 "focus" = (if scroll then B.visible else id) . B.withDefAttr "focus"
    mod1 attr    = B.withDefAttr (B.attrName attr)

highlightSearch
  :: Name    -- ^ viewport name
  -> Int     -- ^ occurrences so far
  -> String  -- ^ the whole string (haystack)
  -> String  -- ^ the string to search for (needle)
  -> ( Widget Name  -- resulting widget
     , Int          -- updated number of occurrences
     )
highlightSearch vn n0 s0 toS =
  case toS of
    []    -> (str s0,  n0)
    (c:_) -> first hBox $ go n0 (find s0 (findIndices (== c) s0))
  where
    go n []       = ([], n)
    go n (x : xs) = first (x' :) $ go n' xs
      where
        (x', n') = case x of
          Left  s -> (str s, n)
          Right s -> ( ( B.showCursor (SearchResult vn n) (B.Location (1,0))
                       $ B.forceAttr "search"
                       $ str s )
                     , n + 1 )

    find :: String -> [Int] -> [Either String String]
    find s []     = [Left s]
    find s (i:is)
      | i > 0 = Left (take i s) : find (drop i s) ((\j -> j - i) <$> (i:is))

      | Just s' <- toS `getPrefix` s
      = Right toS : find s' ((\j -> j - length toS) <$> is)

      | otherwise
      = find s is

    getPrefix []    s       = Just s
    getPrefix _     []      = Nothing
    getPrefix (c:s) (c':s') | c == c'   = getPrefix s s'
                            | otherwise = Nothing

emph :: String -> Widget n
emph = B.withAttr "emph" . str

title :: String -> Widget n
title = B.withAttr "title"  . str . (" " ++) . (++ " ")

withBorder, withBorderSelected :: String -> Widget n -> Widget n
withBorder           = withBorderStyle BrS.unicode
withBorderSelected   = withBorderStyle BrS.unicodeBold

withBorderStyle :: BrS.BorderStyle -> String -> Widget n -> Widget n
withBorderStyle style s w =
    B.withBorderStyle style
  $ Br.borderWithLabel (title s)
  $ B.padAll 1
  $ w

fillSize :: Int -> String -> String
fillSize n s = replicate l ' ' ++ s ++ replicate (l + r) ' '
  where (l, r) = (n - length s) `quotRem` 2

vBoxSpaced :: [Widget n] -> Widget n
vBoxSpaced []     = B.emptyWidget
vBoxSpaced (w:ws) = vBox $ w : (B.padTop (B.Pad 1) <$> ws)

hBoxSpaced :: Int -> [Widget n] -> Widget n
hBoxSpaced _ []     = B.emptyWidget
hBoxSpaced n (w:ws) = hBox $ w : (B.padLeft (B.Pad n) <$> ws)
