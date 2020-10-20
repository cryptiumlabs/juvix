module NameSymb where

import qualified Juvix.Core.Common.NameSymbol as NameSymbol
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as T

--------------------------------------------------------------------------------
-- Top Level Test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "NameSymbol tests:"
    [idIsId]

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

id :: Symbol -> Symbol
id = NameSymbol.toSymbol . NameSymbol.fromSymbol

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

idIsId :: T.TestTree
idIsId =
  T.forAll (T.listOf T.arbitraryUnicodeChar) (appenDot . intern)
    |> T.testProperty "toSymbol and fromSymbol are inverses"

--------------------------------------------------------------------------------
-- property Helpers
--------------------------------------------------------------------------------

appenDot :: Symbol -> T.Property
appenDot symb =
  eq symb T..&&. eq dotEnd T..&&. eq dotStart T..&&. eq dotMiddle
  where
    eq s = id s T.=== s
    --
    dotEnd = symb <> "."
    dotStart = "." <> symb
    dotMiddle = symb <> "." <> symb
