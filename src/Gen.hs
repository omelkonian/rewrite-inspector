{-|
  Copyright   :  (C) 2019, QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  Orestis Melkonian <melkon.or@gmail.com>

  Generic interface for Term types.
-}
{-# LANGUAGE TemplateHaskell #-}

module Gen where

import GHC.Generics (Generic)

import Data.Binary  (Binary, decodeFile)

import Data.Text.Prettyprint.Doc.Internal (Doc (..))

import qualified Graphics.Vty as V

import Lens.Micro.TH (makeLenses)

-- | Program binders (i.e. top-level identifiers) are identified by their name.
type Binder = String

-- | Syntactic annotations used for highlighting.
-- This should be stored in the pretty-printed code output,
-- in addition to term contexts.
data Syntax
  = Type
  -- ^ type information
  | Keyword
  -- ^ standard keywords of the language
  | Literal
  -- ^ literal values (e.g. strings, numbers)
  | Unique
  -- ^ unique identifiers
  | Qualifier
  -- ^ qualifiers for modules
  | Custom String
  -- ^ used for user-supplied styling

instance Show Syntax where
  show = \case
    Type      -> "type"
    Keyword   -> "keyword"
    Literal   -> "literal"
    Unique    -> "unique"
    Qualifier -> "qualifier"
    Custom s  -> s

class IsoFlags a where
  toFlags :: a -> [Bool]
  fromFlags :: [Bool] -> a

-- | This is the typeclass that the user-supplied @term@ type should implement.
-- It requires all operations, which are necessary for our TUI to runn.
class ( Eq (Ctx term), Binary (Ctx term)
      , Binary (Ann term)
      , IsoFlags (Options term)
      ) => Diff term where
  -- | The type of annotations associated to the given @term@ type.
  type Ann     term :: *
  -- | The type of options for the associated pretty-printer for @term@.
  type Options term :: *
  -- | The type of navigation contexts for values of type @term@.
  type Ctx     term :: *

  -- | Read a rewrite history from a binBooary file on disk.
  readHistory :: FilePath -> IO (History term (Ctx term))

  default readHistory :: (Binary term, Binary (Ctx term))
                      => FilePath -> IO (History term (Ctx term))
  readHistory = decodeFile

  -- | Given a rewrite history, extract the top-level initial expression.
  initialExpr :: History term (Ctx term) -> term
  initialExpr = _before . last

  -- | If a binder containing this name exists, display first in the list of binders.
  topEntity :: String
  topEntity = "top"

  -- | Handle annotations of the pretty-printed code,
  -- emitting either syntax elements or navigation contexts.
  handleAnn :: Ann term -> Either Syntax (Ctx term)

  default handleAnn :: Ann term ~ Ctx term => Ann term -> Either Syntax (Ctx term)
  handleAnn = Right

  -- | User-supplied styling for the TUI.
  userStyles :: [(String, V.Attr)]
  userStyles = []

  -- | Provide the boolean flags of the pretty-printing options.
  -- NB: Lenses are not used here, due to impredicativity...
  flagFields :: [( Options term -> Bool                  -- getter
                 , Options term -> Bool -> Options term  -- setter
                 , String                                -- text to display
                 )]
  flagFields = []

  -- | Initial options for the pretty-printer.
  initOptions :: Options term
  initOptions = fromFlags $ replicate (length $ flagFields @term) True

  -- | Pretty-print a given expression, given some options.
  -- The resulting document format should contain syntax/context annotations.
  ppr' :: Options term -> term -> Doc (Ann term)

  ppr :: term -> Doc (Ann term)
  ppr = ppr' (initOptions @term)

  -- | Patch a given expression, given a navigation context to a sub-expression
  -- and a new sub-expression to replace it.
  patch :: term -> [Ctx term] -> term -> term

-- * Rewrite history.

-- | The rewrite history consists of multiple single steps of rewriting.
type History term ctx = [HStep term ctx]

-- | Each step of the rewrite history contains information about a single rewrite.
data HStep term ctx = HStep
  { _ctx    :: [ctx]
  -- ^ the current context of the sub-expression being rewritten
  , _bndrS  :: String
  -- ^ the name of the current binder
  , _name   :: String
  -- ^ the name of the applied transformation
  , _before :: term
  -- ^ the sub-expression __before__ rewriting
  , _after  :: term
  -- ^ the sub-expression __after__ rewriting
  } deriving (Generic, Show, Binary)
makeLenses ''HStep
