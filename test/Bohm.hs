module Bohm where

import           Juvix.Backends.Env
import           Juvix.Backends.Graph
import           Juvix.Backends.Interface
import           Juvix.Backends.Maps
import           Juvix.Bohm.Parser
import           Juvix.Bohm.Translation
import           Juvix.Bohm.Type
import           Juvix.Bohm.Shared
import           Juvix.Library
import           Juvix.Nets.Bohm
import           Juvix.Utility

import           Juvix.Visualize.Dot
import           Juvix.Visualize.Graph

import           Juvix.EAL

import           Text.Parsec

import           Data.Graph.Inductive     hiding (Network, Node, delNodes,
                                           nodes)

--test1 ∷ Either ParseError (InfoNet (FlipNet Lang))
test1 ∷ Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang) Primitive)
test1 = runMapNet (reduceAll 10 >> findEdge (1, Aux1)) . astToNet <$> parseBohm "(lambda x. x)"

test1' :: Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang) Primitive)
test1' = runMapNet (reduceAll 1) . astToNet <$> parseBohm "((lambda x. x) y)"

parsed ∷ Network net ⇒ Either ParseError (net Lang)
parsed = astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2' ∷ Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang) Primitive)
test2' = runMapNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2 ∷ Either ParseError (InfoNet (FlipNet Lang) Primitive)
test2 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test3 ∷ Either ParseError (InfoNet (FlipNet Lang) Primitive)
test3 = runFlipNet (reduceAll 1) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test4 ∷ Either ParseError (InfoNet (FlipNet Lang) Primitive)
test4 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(lambda y. (lambda x. (y x)) (lambda x. 2 + x))"

test5 ∷ Either ParseError (InfoNet (FlipNet Lang) Primitive)
test5 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(2 + 2)"

test6 ∷ Either ParseError (InfoNet (FlipNet Lang) Primitive)
test6 = runFlipNet (reduceAll 0) . astToNet <$> parseBohm "( (lambda x. (x + 3 + 5)) 2)"


test7 ∷ Maybe Bohm
test7 = testAst $ runFlipNet (reduceAll 10) . astToNet <$> (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test7' ∷ Maybe Bohm
test7' = testAst $ runMapNet (reduceAll 10) . astToNet <$> (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")

testBlah ∷ Either ParseError (Maybe (Adj EdgeInfo), InfoNet (FlipNet Lang) Primitive)
testBlah = runFlipNet' (do reduceAll 10
                           net ← get @"net"
                           return $ fmap lneighbors' $ fst $ match 3 (runFlip net)
                        ) . astToNet <$> (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test6Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang) Primitive))
test6Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet) (parseBohm "( (lambda x. (x + 3 + 5)) 2)")

test67Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang) Primitive))
test67Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet) (parseBohm "( (lambda x. (x + y + y)) 2)")

-- run these on any of the tests above
-- gives back a term for all except for Omega, but that is reasonable
testAst ∷ DifferentRep net ⇒ Either a (InfoNet (net Lang) Primitive) → Maybe Bohm
testAst (Right (InfoNet {net = n})) = netToAst n
testAst (Left _)                    = Nothing


test78Back ∷ Maybe Juvix.Bohm.Type.Bohm
test78Back = netToAst n
  where
    Right (InfoNet {net = n}) =
      fmap (runFlipNet (reduceAll 100) . astToNet)
           (parseBohm "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

-- TODO ∷ run Net → Ast with this, and see if it gives back a church 2!
test8Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang) Primitive))
test8Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet)
                    (parseBohm "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")


test9Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang) Primitive))
test9Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet)
                    (parseBohm "(lambda s . (lambda z . (s (s z))))")

test10Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang) Primitive))
test10Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet)
                     (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")


printTest3 ∷ IO ()
printTest3 = showNet "test3.dot" (runFlip net)
  where
    Right (InfoNet {net = net}) = test3
