module VStack where

import qualified Data.Set as Set
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.Compilation.VirtualStack as VStack
import Juvix.Library
import qualified Juvix.Library.Usage as Usage
import qualified Michelson.Untyped as Untype
import qualified Michelson.Untyped.Type as T
import qualified Michelson.Untyped.Value as Untyped
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

--------------------------------------------------------------------------------
-- Top Level Test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "VStack tests:"
    [ lookupX1,
      lookupX1Free,
      lookupX2Free,
      updateUsageHolds,
      updateUsageOnlyUpdatesFirst,
      dropPropogatesUsage,
      dropDoesNotProp1Usage,
      dropPosNPropagatesBackwords,
      dropPosNPropagatesForwards,
      namingNamesAllWithSame,
      namingNamesDoesNotChangeUsage,
      namingValAddsUsage
    ]

--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

lookupX1 :: T.TestTree
lookupX1 =
  T.testCase
    "Lookup x with usage 1 should be found"
    (VStack.lookup "x" (xIsNotFree mempty) T.@=? Just (foundSave one 0))

lookupX1Free :: T.TestTree
lookupX1Free =
  T.testCase
    "LookupFree x with usage 1 should-NOT be found"
    (VStack.lookupFree "x" (xIsNotFree mempty) T.@=? Nothing)

updateUsageHolds :: T.TestTree
updateUsageHolds =
  T.testCase
    "updateUsage should properly add to the first value"
    ( VStack.lookupFree
        "x"
        (VStack.updateUsage (Set.singleton "x") one (xIsNotFree mempty))
        T.@=? Just (foundSave (Usage.SNat 2) 0)
    )

updateUsageOnlyUpdatesFirst :: T.TestTree
updateUsageOnlyUpdatesFirst =
  T.testCase
    "updateUsage should properly add to the first value"
    ( VStack.lookupAllPos "x" (xIsNowFree (xIsNotFree mempty))
        T.@=? [foundSave (Usage.SNat 2) 0, foundSave (Usage.SNat 1) 1]
    )

lookupX2Free :: T.TestTree
lookupX2Free =
  T.testCase
    "LookupFree x with usage 2 should be found"
    ( VStack.lookupFree "x" (xIsNowFree mempty)
        T.@=? Just (foundSave (Usage.SNat 2) 0)
    )

dropPropogatesUsage :: T.TestTree
dropPropogatesUsage =
  xIsNotFree mempty
    |> xIsNowFree
    |> VStack.drop 1
    |> VStack.lookup "x"
    |> (T.@=? Just (foundSave (Usage.SNat 2) 0))
    |> T.testCase
      "dropping should update usage"

dropDoesNotProp1Usage :: T.TestTree
dropDoesNotProp1Usage =
  xIsNotFree mempty
    |> xIsNotFree
    |> VStack.drop 1
    |> VStack.lookup "x"
    |> (T.@=? Just (foundSave (Usage.SNat 1) 0))
    |> T.testCase
      "dropping 0 or 1 shouldn't propagate usage"

dropPosNPropagatesForwards :: T.TestTree
dropPosNPropagatesForwards =
  xIsNowFree mempty
    |> xIsNowFree
    |> VStack.dropPos 1
    |> VStack.lookup "x"
    |> (T.@=? Just (foundSave (Usage.SNat 4) 0))
    |> T.testCase
      "dropping pos of the first var, gives to the back usages"

dropPosNPropagatesBackwords :: T.TestTree
dropPosNPropagatesBackwords =
  xIsNowFree mempty
    |> xIsNowFree
    |> VStack.dropPos 0
    |> VStack.lookup "x"
    |> (T.@=? Just (foundSave (Usage.SNat 4) 0))
    |> T.testCase
      "dropping pos of the last var, gives to the front usages"

namingNamesAllWithSame :: T.TestTree
namingNamesAllWithSame =
  let stack =
        xIsNotFree mempty
          |> xIsNotFree
          |> VStack.nameTop "y" one
   in ( VStack.lookupAllPos "y" stack == [foundSave one 0, foundSave one 1]
          && VStack.lookupAllPos "x" stack == [foundSave one 0, foundSave one 1]
          T.@=? True
      )
        |> T.testCase "naming a named var, propagates a new name properly"

namingNamesDoesNotChangeUsage :: T.TestTree
namingNamesDoesNotChangeUsage =
  xIsNotFree mempty
    |> xIsNotFree
    |> VStack.nameTop "y" one
    |> VStack.lookup "y"
    |> (T.@=? Just (foundSave one 0))
    |> T.testCase "naming a named var, doesn't change usage"

namingValAddsUsage :: T.TestTree
namingValAddsUsage =
  xIsNotFree mempty
    |> threeOnStack
    |> VStack.nameTop "y" (Usage.SNat 5)
    |> VStack.car
    |> ( T.@=?
           ( VStack.VarE
               (Set.singleton "y")
               (VStack.Usage (Usage.SNat 5) False)
               (Just (VStack.ConstE (Untyped.ValueInt 3))),
             unit
           )
       )
    |> T.testCase "naming a Val adds usage"

--------------------------------------------------------------------------------
-- Creation Helpers
--------------------------------------------------------------------------------

threeOnStack :: VStack.T lamType -> VStack.T lamType
threeOnStack = VStack.cons (VStack.Val (int 3), unit)

-- we update the previous xIsNotFree explicitly to test behavior
xIsNowFree :: VStack.T Int -> VStack.T Int
xIsNowFree =
  VStack.updateUsage (Set.singleton "x") one . xIsNotFree

xIsNotFree :: VStack.T Int -> VStack.T Int
xIsNotFree =
  VStack.cons
    (VStack.VarE (Set.singleton "x") (VStack.Usage one True) Nothing, unit)

unit :: Untype.Type
unit = T.Type T.TUnit ""

only3 :: Num lamType => VStack.T lamType
only3 = VStack.cons (VStack.Val (VStack.LamPartialE 3), unit) mempty

single3 :: VStack.T lamType
single3 = VStack.cons (VStack.Val (int 3), unit) mempty

int :: Integer -> VStack.Val lamType
int x = VStack.ConstE (Untyped.ValueInt x)

foundSave :: Usage.T -> Natural -> VStack.Lookup lamType
foundSave usage pos = VStack.Position (VStack.Usage usage True) pos
