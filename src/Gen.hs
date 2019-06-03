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

--------------------------------------------------
-- Syntactic annotations used for highlighting.

data Syntax
  = Type           -- ^ type information
  | Keyword        -- ^ standard keywords of the language
  | Literal        -- ^ literal values (e.g. strings, numbers)
  | Unique         -- ^ unique identifiers
  | Qualifier      -- ^ qualifiers for modules
  | Custom String  -- ^ used for user-supplied styling

instance Show Syntax where
  show = \case
    Type      -> "type"
    Keyword   -> "keyword"
    Literal   -> "literal"
    Unique    -> "unique"
    Qualifier -> "qualifier"
    Custom s  -> s

--------------------------------------------------
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

  handleAnn :: Ann term -> Either Syntax (Ctx term)

  default handleAnn :: Ann term ~ Ctx term => Ann term -> Either Syntax (Ctx term)
  handleAnn = Right

  userStyles :: [(String, V.Attr)]
  userStyles = []

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

--------------------------------------------------
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
