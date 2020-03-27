module INet where

import Data.Graph.Inductive hiding
  ( Network,
    Node,
    delNodes,
    nodes,
  )
import Juvix.Core.EAC
import qualified Juvix.Core.Parameterisations.Unit as Unit
import Juvix.Interpreter.InteractionNet
import Juvix.Interpreter.InteractionNet.Backends.Env
import Juvix.Interpreter.InteractionNet.Backends.Graph
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.Backends.Maps
import Juvix.Interpreter.InteractionNet.Nets.Default
import Juvix.Interpreter.InteractionNet.Type
import Juvix.Library
import Juvix.Visualize.Dot
import Juvix.Visualize.Graph
import Text.Parsec

type PrimVal = Unit.Val

ealToINet :: forall primVal. RPTO primVal -> AST primVal
ealToINet = erasedCoreToInteractionNetAST . erase

astToNetDefault :: Network net => AST PrimVal -> net (Lang PrimVal)
astToNetDefault net = astToNet Unit.t net defaultEnv

--test1 ∷ Either ParseError (InfoNet (FlipNet Lang))
test1 :: Either ParseError (InfoNet (Juvix.Interpreter.InteractionNet.Backends.Maps.Net (Lang PrimVal)))
test1 =
  runMapNet (reduceAll 10 >> findEdge (1, Aux1)) . astToNetDefault
    <$> parseAST "(lambda x. x)"

test1' :: Either ParseError (InfoNet (Juvix.Interpreter.InteractionNet.Backends.Maps.Net (Lang PrimVal)))
test1' = runMapNet (reduceAll 1) . astToNetDefault <$> parseAST "((lambda x. x) y)"

parsed :: Network net => Either ParseError (net (Lang PrimVal))
parsed = astToNetDefault <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test2' :: Either ParseError (InfoNet (Juvix.Interpreter.InteractionNet.Backends.Maps.Net (Lang PrimVal)))
test2' =
  runMapNet (reduceAll 10) . astToNetDefault
    <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test2 :: Either ParseError (InfoNet (FlipNet (Lang PrimVal)))
test2 =
  runFlipNet (reduceAll 10) . astToNetDefault
    <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test3 :: Either ParseError (InfoNet (FlipNet (Lang PrimVal)))
test3 =
  runFlipNet (reduceAll 1) . astToNetDefault
    <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test4 :: Either ParseError (InfoNet (FlipNet (Lang PrimVal)))
test4 =
  runFlipNet (reduceAll 10)
    . astToNetDefault
    <$> parseAST "(lambda y. (lambda x. (y x)) (lambda x. 2 + x))"

test5 :: Either ParseError (InfoNet (FlipNet (Lang PrimVal)))
test5 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseAST "(2 + 2)"

test5' :: Either ParseError (InfoNet (FlipNet (Lang PrimVal)))
test5' = runFlipNet (reduceAll 10) . astToNetDefault <$> parseAST "(plus 2 2)"

test6 :: Either ParseError (InfoNet (FlipNet (Lang PrimVal)))
test6 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseAST "( (lambda x. (x + 3 * 5)) 2)"

test7 :: Maybe (AST PrimVal)
test7 =
  testAst $
    runFlipNet (reduceAll 10)
      . astToNetDefault <$> (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test7' :: Maybe (AST PrimVal)
test7' =
  testAst $
    runMapNet (reduceAll 10) . astToNetDefault
      <$> (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

testBlah :: Either ParseError (Maybe (Adj EdgeInfo), InfoNet (FlipNet (Lang PrimVal)))
testBlah =
  runFlipNet'
    ( do
        reduceAll 10
        net <- get @"net"
        return $ fmap lneighbors' $ fst $ match 3 (runFlip net)
    )
    . astToNetDefault
    <$> (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test6Gen :: IO (Either ParseError (InfoNet (FlipNet (Lang PrimVal))))
test6Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "( (lambda x. (x + 3 + 5)) 2)")

test67Gen :: IO (Either ParseError (InfoNet (FlipNet (Lang PrimVal))))
test67Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "( (lambda x. (x + y + y)) 2)")

testGen :: IO (Either ParseError (InfoNet (FlipNet (Lang PrimVal))))
testGen = traverse (netToGif "tmp/" "boo" 1000 . astToNetDefault) (parseAST "((lambda x. (x x)) 2)")

-- run these on any of the tests above
-- gives back a term for all except for Omega, but that is reasonable
testAst :: DifferentRep net => Either a (InfoNet (net (Lang PrimVal))) -> Maybe (AST PrimVal)
testAst (Right (InfoNet {net = n})) = netToAst n
testAst (Left _) = Nothing

test78Back :: Maybe (Juvix.Interpreter.InteractionNet.Type.AST PrimVal)
test78Back = netToAst n
  where
    Right (InfoNet {net = n}) =
      fmap
        (runFlipNet (reduceAll 100) . astToNetDefault)
        (parseAST "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

-- TODO ∷ run Net → Ast with this, and see if it gives back a church 2!
test8Gen :: IO (Either ParseError (InfoNet (FlipNet (Lang PrimVal))))
test8Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

test9Gen :: IO (Either ParseError (InfoNet (FlipNet (Lang PrimVal))))
test9Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "(lambda s . (lambda z . (s (s z))))")

test10Gen :: IO (Either ParseError (InfoNet (FlipNet (Lang PrimVal))))
test10Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

printTest3 :: IO ()
printTest3 = showNet "test3.dot" (runFlip net)
  where
    Right (InfoNet {net = net}) = test3
