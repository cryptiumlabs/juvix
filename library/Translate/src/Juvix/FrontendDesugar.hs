-- |
-- - Order of Passes
--   1. =RemoveModule=
--   2. =RemoveGuard=
--   3. =RemoveCond=
--   4. =CombineMultiple=
--   5. =RemoveSignature=
--   6. =RemovePunned=
--   7. =RemoveDo=
module Juvix.FrontendDesugar where

import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types as Initial
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp

op :: [Initial.TopLevel] -> [Sexp.T]
op = Desugar.op . fmap SexpTrans.transTopLevel
