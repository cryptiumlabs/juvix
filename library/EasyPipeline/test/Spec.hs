import qualified FromFrontend
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Juvix.Library

main :: IO ()
main = T.defaultMain FromFrontend.top
