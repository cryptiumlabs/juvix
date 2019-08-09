{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Juvix.Encoding.Scott where

import           Prelude         (error)

import           Juvix.Library   hiding (Sum, Product)
import           Juvix.Encoding.Types
import           Juvix.Encoding.Encoding


adtToScott  ∷ ( HasState "constructors" (Map SomeSymbol Bound)    m
              , HasState "adtMap"       (Map SomeSymbol Branches) m
              , HasThrow "err"          Errors                    m )
            ⇒ Name → m ()
adtToScott (Adt name s) = sumRec s 1 (adtLength s)
  where
    adtLength Single {}      = 1
    adtLength (Branch _ _ s) = succ (adtLength s)

    sumRec (Branch s p next) posAdt lenAdt = do
      adtConstructor s (sumProd p posAdt lenAdt) name
      sumRec next (succ posAdt) lenAdt
    sumRec (Single s p) posAdt lenAdt =
      adtConstructor s (sumProd p posAdt lenAdt) name

    sumProd None posAdt lengthAdt = generateLam posAdt lengthAdt identity
    sumProd Term posAdt lengthAdt =
      Lambda (someSymbolVal "%arg1")
             (generateLam posAdt
                          lengthAdt
                          (\b → Application b (Value $ someSymbolVal "%arg1")))
    sumProd p@(Product _) posAdt lengthAdt = args (lengthProd p)
      where
        args prodLen = foldr (\spot → Lambda (someSymbolVal $ "%arg" <> show spot))
                             (encoding prodLen)
                             [1..prodLen]
        encoding prodLen = generateLam posAdt lengthAdt $
          \body →
            foldl (\acc i → Application acc (Value $ someSymbolVal
                                                   $ "%arg" <> show i))
                  body
                  [1..prodLen]
        lengthProd (Product p) = succ (lengthProd p)
        lengthProd Term        = 1
        lengthProd None        = 0 -- I'm skeptical this case ever happens

    generateLam posAdt lengthAdt onPosAdt =
      foldr (\spot → Lambda (someSymbolVal $ "%genArg" <> show spot))
            (onPosAdt $ Value (someSymbolVal $ "%genArg" <> show posAdt))
            [1..lengthAdt]

scottCase ∷ ( HasState "constructors" (Map SomeSymbol Bound)    m
            , HasState "adtMap"       (Map SomeSymbol Branches) m
            , HasThrow "err"          Errors                    m
            , HasWriter "missingCases" [SomeSymbol]             m )
          ⇒ Switch → m Lambda
scottCase c = do
  -- expandedCase ends up coming out backwards in terms of application
  -- we fix this by inverting the application stack making on called on everything
  expandedCase ← caseGen c onNoArg onrec
  case expandedCase of
    Application on b → pure $ reverseApp on b
    Lambda {}        → error "doesn't happen"
    Value  {}        → error "doesn't happen"
  where
    onNoArg        = identity
    onrec c accLam = (Application c accLam)

    reverseApp on (Application b1 b2) = reverseApp (Application on b1) b2
    reverseApp on s                   = Application on s
