{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Precompute parts of the UI state offline.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Precompute where

import GHC.Generics (Generic)

import Control.Arrow             (first)
import Data.Binary               (Binary (..), Get)
import Data.List                 (groupBy, sortOn)
import Data.Text.Prettyprint.Doc ( layoutPretty
                                 , LayoutOptions (..)
                                 , PageWidth (..), SimpleDocStream (..) )

import qualified Data.Vector  as V

import Lens.Micro    ((^.))
import Lens.Micro.TH (makeLenses)

import Gen

instance Binary ann => Binary (SimpleDocStream ann) where
  put = \case
    SFail        -> put "SFail"
    SEmpty       -> put "SEmpty"
    SChar c d    -> put "SChar" >> put c >> put d
    SText n t d  -> put "SText" >> put n >> put t >> put d
    SLine n d    -> put "SLine" >> put n >> put d
    SAnnPush a d -> put "SAnnPush" >> put a >> put d
    SAnnPop d    -> put "SAnnPop" >> put d

  get = do
    constr <- get :: Get String
    case constr of
      "SFail"    -> pure SFail
      "SEmpty"   -> pure SEmpty
      "SChar"    -> SChar    <$> get <*> get
      "SText"    -> SText    <$> get <*> get <*> get
      "SLine"    -> SLine    <$> get <*> get
      "SAnnPush" -> SAnnPush <$> get <*> get
      "SAnnPop"  -> SAnnPop  <$> get
      _          -> error "[get] unknown constructor tag"

instance Binary a => Binary (V.Vector a) where
  put = put . V.toList
  get = V.fromList <$> get

-- | Necessary information to display a rewrite (at each step, at each binder)
data DiffScreen ann ctx = DiffScreen
  { _ctx'    :: [ctx]
  , _name'   :: String
  , _before' :: SimpleDocStream ann
  , _after'  :: SimpleDocStream ann
  } deriving (Generic, Show, Binary)
makeLenses ''DiffScreen


-- | The precomputed states of the terminal UI.
type StatesV ann ctx
  = V.Vector -- binders
      ( V.Vector  -- binder steps
        ( V.Vector  -- available widths
          ( V.Vector  -- available pretty-printing options
            (DiffScreen ann ctx) -- diff
          )
        )
      )

-- | The precomputed state, plus some auxiliary information.
type PrecomputedState ann ctx
  = ( StatesV ann ctx
    , [Binder]
    )

-- | Available code line widths.
type Width = Int

widths :: [Width]
widths = [30, 40, 50, 60, 70, 80, 90, 100]

indexWidth :: Width  -- ^ the current available width
           -> Int    -- ^ return an index into widths
indexWidth w0 = go' widths 0
  where go' []     n = min n (length widths - 1)
        go' (w:ws) n | w >= w0   = n
                     | otherwise = go' ws (n + 1)

-- | Available pretty-printing options.
ppOptions :: forall term. Diff term => [Options term]
ppOptions = fromFlags <$> allFlags (length $ flagFields @term)
  where
    allFlags :: Int -> [[Bool]]
    allFlags 0 = [[]]
    allFlags n = let bs = allFlags (n - 1)
                 in ((True :) <$> bs) ++ ((False :) <$> bs)

indexPP :: forall term. Diff term
        => Options term -> Int
indexPP opts = go 0 (ppOptions @term)
  where
    go _ []       = error "[indexPP] no such option"
    go n (o : os) =
      if toFlags opts == toFlags o
        then n
        else go (n - 1) os

(!!!)
  :: forall term. Diff term
  => PrecomputedState (Ann term) (Ctx term)
  -> (Int, Int, Options term, Width)
  -> DiffScreen (Ann term) (Ctx term)
(st, _) !!! (i, j, opts, w) = st V.! i V.! j V.! indexWidth w V.! indexPP @term opts

precompute
  :: forall term. Diff term
  => History term (Ctx term)                 -- ^ the recorded history of rewrites
  -> PrecomputedState (Ann term) (Ctx term)  -- ^ precomputed expressions
precompute
  = -- :: Vector (Vector (Vector (Vector DiffScreen))) ~ StatesV  , [Binder]
    first ( V.fromList
          . fmap V.fromList
          . fmap (fmap V.fromList)
          . fmap (fmap (fmap V.fromList))
          )
    -- :: [[[[DiffScreen]]]], [Binder]
  . unzip
    -- :: [ [[[DiffScreen]]] , Binder ]
  . map (\h -> (inline (initialExpr h) h , (head h ^. bndrS)))
    -- :: [[HStep]] (grouped by binder)
  . groupBy (\ x y -> x^.bndrS == y^.bndrS)
    -- :: [HStep] (ordered by binder name)
  . sortOn _bndrS
    -- :: [HStep]

  where
    inline :: term
           -> [HStep term (Ctx term)]
           -> [[[DiffScreen (Ann term) (Ctx term)]]]
    inline top = snd . foldl inline' (top, [])

    inline' :: (term, [[[DiffScreen (Ann term) (Ctx term)]]])
            -> HStep term (Ctx term)
            -> (term, [[[DiffScreen (Ann term) (Ctx term)]]])
    inline' (e, t) step = (e', t ++ [d])
      where
        e' = patch e (step^.ctx) (step^.after)
        d  = [ [ DiffScreen { _ctx'    = step^.ctx
                            , _name'   = step^.name
                            , _before' = pr e
                            , _after'  = pr e' }
               | opts <- ppOptions @term
               , let pr = layoutPretty (LayoutOptions (AvailablePerLine w 0.8))
                        . ppr' @term opts
               ]
             | w <- widths
             ]
