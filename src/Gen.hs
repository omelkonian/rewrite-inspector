{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Generic interface for Term types.
-}
{-# LANGUAGE TemplateHaskell #-}
module Gen where

import GHC.Generics (Generic)

import Data.Binary               (Binary, decodeFile)
import Data.Default              (Default, def)
import Data.Text.Prettyprint.Doc (Doc)

import Lens.Micro.TH (makeLenses)

import qualified Graphics.Vty as V

-------------------------------
-- Generic interface.

class Eq (Ctx term) => Diff term where
  type Ann     term :: *
  type Options term :: *
  type Ctx     term :: *

  readHistory :: FilePath -> IO (History term (Ctx term))

  default readHistory :: (Binary term, Binary (Ctx term))
                      => FilePath -> IO (History term (Ctx term))
  readHistory = decodeFile

  initialExpr :: History term (Ctx term) -> term
  initialExpr = _before . last

  topEntity :: String
  topEntity = "top"

  handleAnn :: Ann term -> Either String (Ctx term)

  default handleAnn :: Ann term ~ Ctx term => Ann term -> Either String (Ctx term)
  handleAnn = Right

  annStyles :: [(String, V.Attr)]
  annStyles = []

  initOptions :: Options term

  default initOptions :: Default (Options term) => Options term
  initOptions = def

  flagFields :: [( Options term -> Bool                  -- getter
                 , Options term -> Bool -> Options term  -- setter
                 , String                                -- text to display
                 )]
  flagFields = []

  ppr' :: Options term -> term -> Doc (Ann term)

  patch :: term -> [Ctx term] -> term -> term


--------------------------------
-- History datatype.

type History term ctx = [HStep term ctx]
data HStep term ctx
  = HStep { _ctx    :: [ctx]
          , _bndrS  :: String
          , _name   :: String
          , _before :: term
          , _after  :: term }
          deriving (Generic, Show, Binary)
makeLenses ''HStep
