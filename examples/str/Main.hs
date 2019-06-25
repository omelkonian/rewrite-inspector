{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>
-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import GHC.Generics (Generic)
import Data.Text.Prettyprint.Doc (annotate, vsep, pretty)

import Gen
import BrickUI (runTerminal)

main :: IO ()
main = runTerminal @Expr "examples/str/theme.ini"

-------------------------------
-- Adhoc instance for Diff.

data Expr = N Int | Expr :+: Expr deriving (Eq, Show)

data ExprContext = L | R deriving (Eq, Show)

instance Diff Expr where
  type Ann     Expr = ExprContext
  type Options Expr = ()
  type Ctx     Expr = ExprContext

  readHistory _ = return [ HStep { _ctx    = [L]
                                 , _bndrS  = "top"
                                 , _name   = "adhocI"
                                 , _before = N 1
                                 , _after  = N 11 :+: N 12
                                 }
                         , HStep { _ctx    = [L, L]
                                 , _bndrS  = "top"
                                 , _name   = "adhocII"
                                 , _before = N 11
                                 , _after  = N 111 :+: N 112
                                 }
                         , HStep { _ctx    = []
                                 , _bndrS  = "top"
                                 , _name   = "normalization"
                                 , _before = N 1 :+: (N 2 :+: N 3)
                                 , _after  = ((N 111 :+: N 112) :+: N 12)
                                         :+: (N 2 :+: N 3)
                                 }
                         ]

  ppr' _    (N n)      = pretty $ case n of {1 -> "one" ; 2 -> "two" ; 3 -> "three" ; _ -> "..."}
  ppr' opts (e :+: e') = vsep $ [ annotate L (ppr' opts e) ]
                             ++ replicate 25 (pretty " ")
                             ++ [pretty "plus"]
                             ++ replicate 25 (pretty " ")
                             ++ [ annotate R (ppr' opts e') ]

  patch _ []     e' = e'
  patch curE (c:cs) e' = let go e = patch e cs e' in
    case (curE, c) of
      (l :+: r, L) -> go l :+: r
      (l :+: r, R) -> l :+: go r
      _            -> error "patch"
