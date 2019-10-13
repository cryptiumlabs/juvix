module Bohm where

import Data.Graph.Inductive hiding
  ( Network,
    Node,
    delNodes,
    nodes,
  )
import Juvix.Backends.Env
import Juvix.Backends.Graph
import Juvix.Backends.Interface
import Juvix.Backends.Maps
import Juvix.Bohm.Default
import Juvix.Bohm.Parser
import Juvix.Bohm.Translation
import Juvix.Bohm.Type
import Juvix.EAC
import Juvix.Library
import Juvix.Nets.Bohm
import Juvix.Utility
import Juvix.Visualize.Dot
import Juvix.Visualize.Graph
import Text.Parsec

astToNetDefault :: Network net => Bohm -> net Lang
astToNetDefault net = astToNet net defaultEnv

--test1 ∷ Either ParseError (InfoNet (FlipNet Lang))
test1 :: Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang))
test1 = runMapNet (reduceAll 10 >> findEdge (1, Aux1)) . astToNetDefault <$> parseBohm "(lambda x. x)"

test1' :: Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang))
test1' = runMapNet (reduceAll 1) . astToNetDefault <$> parseBohm "((lambda x. x) y)"

parsed :: Network net => Either ParseError (net Lang)
parsed = astToNetDefault <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2' :: Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang))
test2' = runMapNet (reduceAll 10) . astToNetDefault <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2 :: Either ParseError (InfoNet (FlipNet Lang))
test2 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test3 :: Either ParseError (InfoNet (FlipNet Lang))
test3 = runFlipNet (reduceAll 1) . astToNetDefault <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test4 :: Either ParseError (InfoNet (FlipNet Lang))
test4 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseBohm "(lambda y. (lambda x. (y x)) (lambda x. 2 + x))"

test5 :: Either ParseError (InfoNet (FlipNet Lang))
test5 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseBohm "(2 + 2)"

test5' :: Either ParseError (InfoNet (FlipNet Lang))
test5' = runFlipNet (reduceAll 10) . astToNetDefault <$> parseBohm "(plus 2 2)"

test6 :: Either ParseError (InfoNet (FlipNet Lang))
test6 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseBohm "( (lambda x. (x + 3 * 5)) 2)"

test7 :: Maybe Bohm
test7 = testAst $ runFlipNet (reduceAll 10) . astToNetDefault <$> (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test7' :: Maybe Bohm
test7' = testAst $ runMapNet (reduceAll 10) . astToNetDefault <$> (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")

testBlah :: Either ParseError (Maybe (Adj EdgeInfo), InfoNet (FlipNet Lang))
testBlah =
  runFlipNet'
    ( do
        reduceAll 10
        net <- get@"net"
        return $ fmap lneighbors' $ fst $ match 3 (runFlip net)
    )
    . astToNetDefault
    <$> (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test6Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test6Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNetDefault) (parseBohm "( (lambda x. (x + 3 + 5)) 2)")

test67Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test67Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNetDefault) (parseBohm "( (lambda x. (x + y + y)) 2)")

-- run these on any of the tests above
-- gives back a term for all except for Omega, but that is reasonable
testAst :: DifferentRep net => Either a (InfoNet (net Lang)) -> Maybe Bohm
testAst (Right (InfoNet {net = n})) = netToAst n
testAst (Left _) = Nothing

test78Back :: Maybe Juvix.Bohm.Type.Bohm
test78Back = netToAst n
  where
    Right (InfoNet {net = n}) =
      fmap
        (runFlipNet (reduceAll 100) . astToNetDefault)
        (parseBohm "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

-- TODO ∷ run Net → Ast with this, and see if it gives back a church 2!
test8Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test8Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseBohm "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

test9Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test9Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseBohm "(lambda s . (lambda z . (s (s z))))")

test10Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test10Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (ealToBohm <$> parseEal "lambda x. (lambda y. (lambda z. z))")

printTest3 :: IO ()
printTest3 = showNet "test3.dot" (runFlip net)
  where
    Right (InfoNet {net = net}) = test3
