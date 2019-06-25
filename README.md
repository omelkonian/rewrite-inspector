# Rewrite Inspector

A terminal user-interface (TUI) for inspecting steps taken by a rewriting process.
Useful for the optimization phase of a compiler, or even evaluators of small languages.

Available on [![Hackage](https://img.shields.io/hackage/v/rewrite-inspector.svg)](http://hackage.haskell.org/package/rewrite-inspector)

## Usage Instructions
To use the library, the user's type of language expressions
must be an instance of the `Diff` typeclass.

Let's see an example for a simple language for arithmetic expressions.
```haskell
data Expr = N Int | Expr :+: Expr deriving (Eq, Show)
```

We first need to give the type of contexts, which navigate to a certain sub-term:
```haskell
data ExprContext = L | R deriving (Eq, Show)
```

We can then give the instance for the `Diff` typeclass, by providing the following:
1. `readHistory`: A way to read the rewrite history from file (for this example, always return a constant history).
2. `ppr'`: A pretty-printing for (for this example, the type of annotations is just the type of contexts)
3. `patch`: A way to patch a given expression at some context.

Below is the complete definition of our instance:
```haskell
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

  ppr' _    (N n)      = pretty n
  ppr' opts (e :+: e') = hsep [ annotate L (ppr' opts e)
                              , "+"
                              , annotate R (ppr' opts e')
                              ]

  patch _ []     e' = e'
  patch curE (c:cs) e' = let go e = patch e cs e' in
    case (curE, c) of
      (l :+: r, L) -> go l :+: r
      (l :+: r, R) -> l :+: go r
      _            -> error "patch"
```

Finally, we are ready to run our TUI to inspect the rewriting steps with proper
highlighting (`.ini` files are used to provide styling directives):
```haskell
main :: IO ()
main = runTerminal @Expr "examples/expr/theme.ini"
```

For more examples, check the ![examples](https://github.com/omelkonian/rewrite-inspector/raw/master/examples/) folder.

## Features
The features depicted below have been recorded on the optimizing phase of the
![Cλash compiler](https://github.com/clash-lang/clash-compiler/).

1. Use `Ctrl-p` to show/hide keyboard controls:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/controls.gif)

2. Syntax highlighting:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/syntax.gif)

3. Navigate through the rewriting steps with highlighted diffs:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/steps.gif)

4. Also navigate through all top-level binders:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/binders.gif)

5. Hide extraneous code output, suc as uniques/types/qualifiers:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/hiding.gif)

6. Individual and grouped scrolling for code panes, both vertically and horizontally:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/scrolling.gif)

7. Move to next transformation with the given step number:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/stepno.gif)

8. Move to next transformation with the given name:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/trans.gif)

9. Search for string occurrences within source code:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/search.gif)

10. Inside code panes, the line width is dynamically adjusted depending on available space:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/dynamic.gif)

11. User-configurable colour theme:
![](https://github.com/omelkonian/rewrite-inspector/raw/master/gifs/theme.gif)
