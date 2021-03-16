module Juvix.Library.PrettyPrint
  ( module Juvix.Library.PrettyPrint,
    module Text.PrettyPrint.Compact
  ) where

import Text.PrettyPrint.Compact.Core
import Text.PrettyPrint.Compact
  hiding (lparen, rparen, langle, rangle, lbrace, rbrace,
          lbracket, rbracket, squote, dquote, semi, colon,
          comma, space, dot, backslash, equals)
import Juvix.Library hiding (show)
import qualified Text.Show as Show
import Prelude (String)


-- | Annotations, which can be used for e.g. syntax highlighting
type family Ann a :: *

data Prec
  -- | Outermost expression; no parens needed
  = Outer
  -- | Argument of infix function, with the same meaning as the argument of
  -- 'Text.Show.showsPrec'
  | Infix Natural
  -- | Argument (or head) of nonfix function; all non-atomic expressions need
  -- parens
  | FunArg
  deriving (Eq, Ord, Show, Generic)

type PrecReader = HasReader "prec" Prec

-- | Class for pretty-printing syntax-like types, which need to keep track of
-- the surrounding precedence level.
class Monoid (Ann a) => PrettySyntax a where
  -- | Pretty-prints a syntax value, given the current precedence value in
  -- a reader environment.
  pretty' :: PrecReader m => a -> m (Doc (Ann a))
  default pretty' ::
    (PrettyText a, PrecReader m) => a -> m (Doc (Ann a))
  pretty' = pure . prettyText

runPretty :: Prec -> (forall m. PrecReader m => m a) -> a
runPretty prec m = let MonadReader x = m in runReader x prec

runPretty0 :: (forall m. PrecReader m => m a) -> a
runPretty0 = runPretty Outer

-- | Pretty-print at the given precedence level.
pretty :: PrettySyntax a => Prec -> a -> Doc (Ann a)
pretty prec x = runPretty prec (pretty' x)

-- | Pretty-print at the initial precedence level.
pretty0 :: PrettySyntax a => a -> Doc (Ann a)
pretty0 = pretty Outer

withPrec :: PrecReader m => Prec -> m a -> m a
withPrec p = local @"prec" \_ -> p

-- | Class for text-like types (e.g. messages), which don't have a concept of
-- precedence.
class Monoid (Ann a) => PrettyText a where
  -- | Pretty-print a value as human-readable text.
  prettyText :: a -> Doc (Ann a)
  default prettyText :: Show a => a -> Doc (Ann a)
  prettyText = text . Show.show


show :: (Monoid ann, Show a) => a -> Doc ann
show = text . Show.show

hsepA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) -> f (Doc ann)
hsepA = fmap hsep . sequenceA . toList

sepA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) -> f (Doc ann)
sepA = fmap sep . sequenceA . toList

hcatA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) -> f (Doc ann)
hcatA = fmap hcat . sequenceA . toList

vcatA ::
  (Applicative f, Foldable t, Monoid ann) =>
  t (f (Doc ann)) -> f (Doc ann)
vcatA = fmap vcat . sequenceA . toList

punctuateA ::
  (Applicative f, Foldable t, Monoid ann) =>
  f (Doc ann) -> t (f (Doc ann)) -> [f (Doc ann)]
punctuateA s = go . toList
  where
    go [] = []
    go [d] = [d]
    go (d:ds) = hcatA [d, s] : go ds

hangA ::
  (Applicative f, Monoid ann) =>
  Int -> f (Doc ann) -> f (Doc ann) -> f (Doc ann)
hangA i = liftA2 $ hang i

-- | Same as 'hangWith', but with multiple hanging elements.
--
-- @
-- >>> hangsWith "*" 2 "hello" ["cool", "world"]
-- hello*cool*world
-- -- or --
-- hello
--   cool
--   world
-- @
hangsWith ::
  (Foldable t, Monoid ann) =>
  String -> Int -> Doc ann -> t (Doc ann) -> Doc ann
hangsWith sep n a bs =
  groupingBy sep $ (0, a) : map (n,) (toList bs)

hangs :: (Foldable t, Monoid ann) => Int -> Doc ann -> t (Doc ann) -> Doc ann
hangs = hangsWith " "

hangsA ::
  (Applicative f, Foldable t, Monoid ann) =>
  Int -> f (Doc ann) -> t (f (Doc ann)) -> f (Doc ann)
hangsA i a bs = hangs i <$> a <*> sequenceA (toList bs)


parensA :: Monoid ann => ann -> Doc ann -> Doc ann
parensA ann = annotate ann "(" `enclose` annotate ann ")"

parensA' :: ann -> Doc (Last ann) -> Doc (Last ann)
parensA' = parensA . Last . Just

parensP ::
  (Monoid ann, PrecReader m) => ann -> Prec -> m (Doc ann) -> m (Doc ann)
parensP ann p d = do
  p' <- ask @"prec"
  if p > p' then d else parensA ann <$> d

parensP' ::
  PrecReader m => ann -> Prec -> m (Doc (Last ann)) -> m (Doc (Last ann))
parensP' = parensP . Last . Just

annotate' :: ann -> Doc (Last ann) -> Doc (Last ann)
annotate' = annotate . Last . Just

app ::
  ( PrecReader m,
    Foldable t,
    Monoid ann
  ) =>
  ann -> m (Doc ann) -> t (m (Doc ann)) -> m (Doc ann)
app ann f xs = parensP ann FunArg $ withPrec FunArg $ hangsA indentWidth f xs

app' ::
  ( PrecReader m,
    Foldable t
  ) =>
  ann -> m (Doc (Last ann)) -> t (m (Doc (Last ann))) -> m (Doc (Last ann))
app' = app . Last . Just

indentWidth :: Int
indentWidth = 2
