{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- - Gives a =Scott= encoding from an =ADT= and works properly for case
--   expressions
-- - Overall this encoding just works™

module Juvix.Encoding.Scott where

import Juvix.Encoding.Encoding
import Juvix.Encoding.Types
import Juvix.Library hiding (Product, Sum)
import qualified Juvix.Library.HashMap as Map
import Prelude (error)

adtToScott ∷
  ( HasState "constructors" (Map.Map Symbol Bound) m,
    HasState "adtMap" (Map.Map Symbol Branches) m,
    HasThrow "err" Errors m
  ) ⇒
  Name →
  m ()
adtToScott (Adt name s) = sumRec s 1 (adtLength s)
  where
    adtLength Single {} = 1
    adtLength (Branch _ _ s) = succ (adtLength s)

    sumRec (Branch s p next) posAdt lenAdt = do
      adtConstructor s (sumProd p posAdt lenAdt) name
      sumRec next (succ posAdt) lenAdt
    sumRec (Single s p) posAdt lenAdt =
      adtConstructor s (sumProd p posAdt lenAdt) name

    sumProd None posAdt lengthAdt = generateLam posAdt lengthAdt identity
    sumProd Term posAdt lengthAdt =
      Lambda
        (intern "%arg1")
        ( generateLam
            posAdt
            lengthAdt
            (\b → Application b (Value $ intern "%arg1"))
        )
    sumProd p@(Product _) posAdt lengthAdt = args (lengthProd p)
      where
        args prodLen =
          foldr
            (\spot → Lambda (intern $ "%arg" <> show spot))
            (encoding prodLen)
            [1 .. prodLen]

        encoding prodLen = generateLam posAdt lengthAdt $
          \body →
            foldl
              ( \acc i →
                  Application
                    acc
                    ( Value $ intern $
                        "%arg" <> show i
                    )
              )
              body
              [1 .. prodLen]

        lengthProd (Product p) = succ (lengthProd p)
        lengthProd Term = 1
        lengthProd None = 0 -- I'm skeptical this case ever happens

    generateLam posAdt lengthAdt onPosAdt =
      foldr
        (\spot → Lambda (intern $ "%genArg" <> show spot))
        (onPosAdt $ Value (intern $ "%genArg" <> show posAdt))
        [1 .. lengthAdt]

scottCase ∷
  ( HasState "constructors" (Map.Map Symbol Bound) m,
    HasState "adtMap" (Map.Map Symbol Branches) m,
    HasThrow "err" Errors m,
    HasWriter "missingCases" [Symbol] m
  ) ⇒
  Switch →
  m Lambda
scottCase c = do
  -- expandedCase ends up coming out backwards in terms of application
  -- we fix this by inverting the application stack making on called on everything
  expandedCase ← caseGen c onNoArg onrec
  case expandedCase of
    Application on b → pure $ reverseApp on b
    Lambda {} → error "doesn't happen"
    Value {} → error "doesn't happen"
  where
    onNoArg = identity

    onrec c accLam = (Application c accLam)

    reverseApp on (Application b1 b2) = reverseApp (Application on b1) b2
    reverseApp on s = Application on s
