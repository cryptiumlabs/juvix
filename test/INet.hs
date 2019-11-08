module INet where

import Data.Graph.Inductive hiding
  ( Network,
    Node,
    delNodes,
    nodes,
  )
import Juvix.Core.EAC
import Juvix.Interpreter.InteractionNet
import Juvix.Interpreter.InteractionNet.Backends.Env
import Juvix.Interpreter.InteractionNet.Backends.Graph
import Juvix.Interpreter.InteractionNet.Backends.Interface
import Juvix.Interpreter.InteractionNet.Backends.Maps
import Juvix.Interpreter.InteractionNet.Default
import Juvix.Interpreter.InteractionNet.Nets.Default
import Juvix.Interpreter.InteractionNet.Parser
import Juvix.Interpreter.InteractionNet.Translation
import Juvix.Interpreter.InteractionNet.Type
import Juvix.Library
import Juvix.Visualize.Dot
import Juvix.Visualize.Graph
import Text.Parsec

ealToINet ∷ ∀ primVal. RPTO primVal → AST
ealToINet = erasedCoreToInteractionNetAST . erase

astToNetDefault ∷ Network net ⇒ AST → net Lang
astToNetDefault net = astToNet net defaultEnv

--test1 ∷ Either ParseError (InfoNet (FlipNet Lang))
test1 ∷ Either ParseError (InfoNet (Juvix.Interpreter.InteractionNet.Backends.Maps.Net Lang))
test1 =
  runMapNet (reduceAll 10 >> findEdge (1, Aux1)) . astToNetDefault
    <$> parseAST "(lambda x. x)"

test1' ∷ Either ParseError (InfoNet (Juvix.Interpreter.InteractionNet.Backends.Maps.Net Lang))
test1' = runMapNet (reduceAll 1) . astToNetDefault <$> parseAST "((lambda x. x) y)"

parsed ∷ Network net ⇒ Either ParseError (net Lang)
parsed = astToNetDefault <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test2' ∷ Either ParseError (InfoNet (Juvix.Interpreter.InteractionNet.Backends.Maps.Net Lang))
test2' =
  runMapNet (reduceAll 10) . astToNetDefault
    <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test2 ∷ Either ParseError (InfoNet (FlipNet Lang))
test2 =
  runFlipNet (reduceAll 10) . astToNetDefault
    <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test3 ∷ Either ParseError (InfoNet (FlipNet Lang))
test3 =
  runFlipNet (reduceAll 1) . astToNetDefault
    <$> parseAST "((lambda x. (x x)) (lambda x. (x x)))"

test4 ∷ Either ParseError (InfoNet (FlipNet Lang))
test4 =
  runFlipNet (reduceAll 10)
    . astToNetDefault
    <$> parseAST "(lambda y. (lambda x. (y x)) (lambda x. 2 + x))"

test5 ∷ Either ParseError (InfoNet (FlipNet Lang))
test5 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseAST "(2 + 2)"

test5' ∷ Either ParseError (InfoNet (FlipNet Lang))
test5' = runFlipNet (reduceAll 10) . astToNetDefault <$> parseAST "(plus 2 2)"

test6 ∷ Either ParseError (InfoNet (FlipNet Lang))
test6 = runFlipNet (reduceAll 10) . astToNetDefault <$> parseAST "( (lambda x. (x + 3 * 5)) 2)"

test7 ∷ Maybe AST
test7 =
  testAst $
    runFlipNet (reduceAll 10)
      . astToNetDefault <$> (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test7' ∷ Maybe AST
test7' =
  testAst $
    runMapNet (reduceAll 10) . astToNetDefault
      <$> (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

testBlah ∷ Either ParseError (Maybe (Adj EdgeInfo), InfoNet (FlipNet Lang))
testBlah =
  runFlipNet'
    ( do
        reduceAll 10
        net ← get @"net"
        return $ fmap lneighbors' $ fst $ match 3 (runFlip net)
    )
    . astToNetDefault
    <$> (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

test6Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang)))
test6Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "( (lambda x. (x + 3 + 5)) 2)")

test67Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang)))
test67Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "( (lambda x. (x + y + y)) 2)")

testGen ∷ IO (Either ParseError (InfoNet (FlipNet Lang)))
testGen = traverse (netToGif "tmp/" "boo" 1000 . astToNetDefault) (parseAST "((lambda x. (x x)) 2)")

-- run these on any of the tests above
-- gives back a term for all except for Omega, but that is reasonable
testAst ∷ DifferentRep net ⇒ Either a (InfoNet (net Lang)) → Maybe AST
testAst (Right (InfoNet {net = n})) = netToAst n
testAst (Left _) = Nothing

test78Back ∷ Maybe Juvix.Interpreter.InteractionNet.Type.AST
test78Back = netToAst n
  where
    Right (InfoNet {net = n}) =
      fmap
        (runFlipNet (reduceAll 100) . astToNetDefault)
        (parseAST "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

-- TODO ∷ run Net → Ast with this, and see if it gives back a church 2!
test8Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang)))
test8Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

test9Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang)))
test9Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (parseAST "(lambda s . (lambda z . (s (s z))))")

test10Gen ∷ IO (Either ParseError (InfoNet (FlipNet Lang)))
test10Gen =
  traverse
    (netToGif "tmp/" "boo" 1000 . astToNetDefault)
    (ealToINet <$> parseEal "lambda x. (lambda y. (lambda z. z))")

printTest3 ∷ IO ()
printTest3 = showNet "test3.dot" (runFlip net)
  where
    Right (InfoNet {net = net}) = test3
