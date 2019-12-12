module Main where

import Control.Exception
import CoreTypechecker
import qualified Juvix.Core.IR as IR
import Juvix.Core.Parameterisations.All as All
import Juvix.Core.Parameterisations.Naturals
import Juvix.Core.Parameterisations.Unit
import Juvix.Core.Types
import Juvix.Core.Usage
import Juvix.Library hiding (identity)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

main ∷ IO ()
main =
  T.defaultMain (shouldCheck nat scombinator scombinatorCompNatTy)
    `Control.Exception.catch` ( \e → do
                                  if e == ExitSuccess
                                    then putByteString "OK"
                                    else putByteString $ "Failed. " <> show IR.TypecheckerLog
                                  Juvix.Library.throwIO e
                              )
