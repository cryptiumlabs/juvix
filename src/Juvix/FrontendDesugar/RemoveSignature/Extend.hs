-- |
-- - This pass changes =Signature=, and =Function=
--   + Belongs to Table
--     | Changed   | Is a Sum Type of      |
--     |-----------+-----------------------|
--     | Function  | TopLevel              |
--     | Signature | +TopLevel+ ∧ Function |
-- - _Signature changes_
--   + We move Signature from TopLevel to Function
--     #+begin_src haskell
--       data Signature
--         = Sig
--             { signatureName :: Symbol
--               -- Was a usage but can't alias for now
--             , signatureUsage :: Maybe Expression
--             , signatureArrowType :: Expression
--             , signatureConstraints :: [Expression]
--             }
--         deriving (Show, Generic, NFData)
--     #+end_src
-- - _Function changes_
--   + Function now looks like
--     #+begin_src haskell
--       data Function
--         = Func Symbol (NonEmpty (FunctionLike Expression)) (Maybe Signature)
--     #+end_src
--     * Namely this version adds a signature to upon the previous pass
module Juvix.FrontendDesugar.RemoveSignature.Extend
  ( module Juvix.FrontendDesugar.RemoveSignature.Extend,
    module Juvix.FrontendDesugar.CombineMultiple.Extend,
  )
where

import qualified Extensible as Extension
import Juvix.Frontend.Types.Base
import Juvix.FrontendDesugar.CombineMultiple.Extend hiding (extendFunction, extendTopLevel)
import qualified Juvix.FrontendDesugar.CombineMultiple.Extend as Ext
import Juvix.Library

extendTopLevel :: ExtTopLevel
extendTopLevel = Ext.extendTopLevel {typeSignature = Nothing}

extendFunction :: Extension.TypeQ -> ExtFunction
extendFunction arg =
  -- have to use the default here as we have a function in the previous code
  defaultExtFunction
    { typeFunc = Nothing,
      typeFunctionX =
        [ ( "Func",
            [ [t|Symbol|],
              [t|NonEmpty (FunctionLike' $arg (Expression' $arg))|],
              [t|Maybe (Signature' $arg)|]
            ]
          )
        ]
    }

-- TODO ∷ once we get a signature for let add that here too
