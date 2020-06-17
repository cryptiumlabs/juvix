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
