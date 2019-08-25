import           Juvix.Bohm.Translation
import           Juvix.Bohm.Parser
import           Juvix.Nets.Bohm
import           Juvix.Backends.Graph
import           Juvix.Backends.Maps
import           Juvix.Backends.Env
import           Juvix.Utility.Helper
import           Juvix.Library
import           Juvix.Backends.Interface

import           Juvix.Visualize.Graph
import           Juvix.Visualize.Dot

import           Text.Parsec

test1 :: Either ParseError (InfoNet (FlipNet Lang))
test1 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(lambda x. (x x) y)"


parsed :: Network net => Either ParseError (net Lang)
parsed = astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2' :: Either ParseError (InfoNet (Juvix.Backends.Maps.Net Lang))
test2' = runMapNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test2 :: Either ParseError (InfoNet (FlipNet Lang))
test2 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test3 :: Either ParseError (InfoNet (FlipNet Lang))
test3 = runFlipNet (reduceAll 1) . astToNet <$> parseBohm "((lambda x. (x x)) (lambda x. (x x)))"

test4 :: Either ParseError (InfoNet (FlipNet Lang))
test4 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(lambda y. (lambda x. (y x)) (lambda x. 2 + x))"

test5 :: Either ParseError (InfoNet (FlipNet Lang))
test5 = runFlipNet (reduceAll 10) . astToNet <$> parseBohm "(2 + 2)"


test6 :: Either ParseError (InfoNet (FlipNet Lang))
test6 = runFlipNet (reduceAll 0) . astToNet <$> parseBohm "( (lambda x. (x + 3 + 5)) 2)"

test6Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test6Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet) (parseBohm "( (lambda x. (x + 3 + 5)) 2)")

test67Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test67Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet) (parseBohm "( (lambda x. (x + y + y)) 2)")



test78Back = netToAst n
  where
    Right (InfoNet {net = n}) = runFlipNet (reduceAll 100) . astToNet <$> (parseBohm "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

-- TODO ∷ run Net → Ast with this, and see if it gives back a church 2!
test8Gen :: IO (Either ParseError (InfoNet (FlipNet Lang)))
test8Gen = traverse (netToGif "tmp/" "boo" 1000 . astToNet) (parseBohm "(lambda x. lambda y. ((lambda z. (z (z y))) (lambda w. (x w))))")

printTest3 :: IO ()
printTest3 = showNet "test3.dot" (runFlip net)
  where
    Right (InfoNet {net = net}) = test3
