import qualified FromFrontend
import Juvix.Library
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

main :: IO ()
main = T.defaultMain FromFrontend.top
