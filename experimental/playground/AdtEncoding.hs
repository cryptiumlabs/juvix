{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import qualified Data.Map.Strict as Map
import           Prelude         (error)
import           Juvix.Library   hiding (Sum, Product)

-- TODO ∷ Split into multiple files AdtEncoding/Encoding, AdtEncoding/Scott
-- AdtEncoding/Mendler to better share and organize code

-- Adt Types -------------------------------------------------------------------

data Product = Product Product
             | Term
             | None
             deriving Show

data Sum = Branch SomeSymbol Product Sum
         | Single SomeSymbol Product
         deriving Show

data Name = Adt SomeSymbol Sum
          deriving Show

-- Object Syntax ---------------------------------------------------------------

-- Replace with bohm later!
data Lambda = Lambda SomeSymbol Lambda
            | Value SomeSymbol
            | Application Lambda Lambda
            deriving Show

-- In the real code bake this adt into Bohm or the Lambda itself!!!!

-- Case x of
-- /| Z   → body
-- /| S n → body
data Switch = Case Lambda [Case]

type Argument = SomeSymbol

type Body = Lambda

data Case = C SomeSymbol [Argument] Body

-- Environment type-------------------------------------------------------------
-- TODO :: Add smaller environments for testing
-- Doesn't matter much here, as all these values are pure
-- All this means is that we waste 1 allocation of a list and a map

-- One function does not require constructor, the other does not have
-- the missingCase writer

type Branches = [SomeSymbol]

data Bound = Bound { lam     :: Lambda
                   -- This is to remember what ADΤ we belong to
                   , adtName :: SomeSymbol
                   } deriving (Show, Generic)

data Env = Env { constructors :: Map SomeSymbol Bound
               -- | adtMap is a mapping between the adt name and the ordered cases thereof
               , adtMap :: Map SomeSymbol Branches
               -- | missingCases represent the missing cases of a match
               , missingCases :: [SomeSymbol]
               } deriving (Show, Generic)

data Errors = AlreadyDefined
            | NotInAdt   SomeSymbol
            | NotInMatch SomeSymbol SomeSymbol
            | AdtNotDefined SomeSymbol
            | MatchWithoutCases
            | InvalidAdt
            deriving Show

newtype EnvS a = EnvS (StateT Env (Except Errors) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "constructors" (Map SomeSymbol Bound)) via
    Field "constructors" () (MonadState (StateT Env (Except Errors)))
  deriving (HasState "adtMap" (Map SomeSymbol Branches)) via
    Field "adtMap" () (MonadState (StateT Env (Except Errors)))
  deriving (HasThrow "err" Errors) via
    MonadError (StateT Env (Except Errors))
  deriving ( HasStream "missingCases" [SomeSymbol]
           , HasWriter "missingCases" [SomeSymbol]) via
    WriterLog (Field "missingCases" () (MonadState (StateT Env (Except Errors))))

runEnvsS :: EnvS a → Either Errors (a, Env)
runEnvsS (EnvS a) = runExcept (runStateT a (Env mempty mempty mempty))

-- Application code ------------------------------------------------------------
adtToScott  ∷ ( HasState "constructors" (Map SomeSymbol Bound)    m
              , HasState "adtMap"       (Map SomeSymbol Branches) m
              , HasThrow "err"          Errors                    m )
            ⇒ Name → m ()
adtToScott (Adt name s) = sumRec s 0 (adtLength s)
  where
    adtLength Single {}      = 1
    adtLength (Branch _ _ s) = succ (adtLength s)

    sumRec (Branch s p next) posAdt lenAdt = adtConstructor s (sumProd p posAdt lenAdt) name
                                           *> sumRec next (succ posAdt) lenAdt
    sumRec (Single s p)      posAdt lenAdt = adtConstructor s (sumProd p posAdt lenAdt) name

    sumProd None posAdt lengthAdt = undefined

    -- TODO ∷ return a continuation on the posAdt after generating lengthAdt number of lambdas
    generateLam posAdt lengthAdt = undefined

adtToMendler ∷ ( HasState "constructors" (Map SomeSymbol Bound)    m
               , HasState "adtMap"       (Map SomeSymbol Branches) m
               , HasThrow "err"          Errors                    m )
             ⇒ Name → m ()
adtToMendler (Adt name s) = sumRec s 0
  where
    sumRec (Single s p) posAdt    = adtConstructor s (sumProd p posAdt) name
    sumRec (Branch s p nextSum) 0 = adtConstructor s (sumProd p 0)      name
                                 *> sumRec nextSum 1
    -- So due to how the algorithm works for inr inl placement
    -- we need to incrase posAdt by 1 to reflect that it is a branch
    -- Note 0 is a special case as it is only inl never an inr
    sumRec (Branch s p nextSum) posAdt = adtConstructor s (sumProd p posAdt) name
                                       *> sumRec nextSum (posAdt + 2)

    sumProd None posAdt = numToIn posAdt (Lambda (someSymbolVal "x")
                                                 (Value $ someSymbolVal "x"))
    sumProd Term posAdt = Lambda (someSymbolVal "%gen1")
                                 (numToIn posAdt (Value (someSymbolVal "%gen1")))
    sumProd p@(Product _) posAdt = lambdas (numToInOp posAdt term)
      where
        (lambdas, term) = rec' p 0 (identity, (Value (someSymbolVal "%fun")))

        rec' x index (lambdasBeforeIn, termToBuild) =
          let genI = someSymbolVal ("%gen" <> show index)
              app  = Application termToBuild (Value genI)
          in case x of
            Term →
              ( lambdasBeforeIn . Lambda genI
              , Lambda (someSymbolVal "%fun") app)
            Product t  → rec' t
                              (succ index)
                              (lambdasBeforeIn . Lambda genI, app)
            None →
              ( lambdasBeforeIn
              , Lambda (someSymbolVal "%fun")
                         (Application
                          termToBuild
                          (Lambda (someSymbolVal "x")
                                  (Value $ someSymbolVal "x"))))


-- Helper for Mendler and Scott encodings --------------------------------------

adtConstructor ∷ ( HasState "adtMap" (Map SomeSymbol [k]) m
         , HasState "constructors" (Map k Bound)  m
         , HasThrow "err" Errors                  m
         , Ord k
         ) ⇒ k → Lambda → SomeSymbol → m ()
adtConstructor s prod name = do
  modify' @"adtMap" (Map.alter (\case
                                   Nothing → Just [s]
                                   Just x  → Just $ x <> [s]) name)
  cons ← get @"constructors"
  case cons Map.!? s of
    Just _  → throw   @"err" AlreadyDefined
    Nothing → modify' @"constructors" (Map.insert s (Bound prod name))

-- Helper for Mendler ----------------------------------------------------------

numToInGen :: Int → Lambda → Lambda
numToInGen 0 arg = app in' arg
numToInGen n arg = app in' (rec' n arg)
  where
    rec' 0 acc = acc
    rec' 1 acc = acc
    rec' n acc = rec' (n - 2) (app inr acc)

-- Here is a chart that lays the relation between branch/single position and the number

-- t      ∧ Pos == 0 ⇒ inl ...             | 0
-- Signle ∧ Pos == 1 ⇒ inr ...             | 1
-- Branch ∧ Pos == 1 ⇒ inr (inl ...)       | 2
-- Single ∧ Pos == 2 ⇒ inr (inr ...)       | 3
-- Branch ∧ Pos == 2 ⇒ inr (inr (inl ...)) | 4

numToIn ∷ Int → Lambda → Lambda
numToIn n arg
  | even n     = numToInGen n (app inl arg)
  | otherwise  = numToInGen n (app inr arg)

numToInOp :: Int → Lambda → Lambda
numToInOp n arg
  | even n     = numToInGen n (app inlOp arg)
  | otherwise  = numToInGen n (app inrOp arg)

userNat :: Name
userNat = Adt (someSymbolVal "Nat")
              (Branch (someSymbolVal "Z") None
                      (Single (someSymbolVal "S") Term))

dUserNat ∷ Name
dUserNat = Adt (someSymbolVal "Nat")
               (Branch (someSymbolVal "Z") None
                       (Branch (someSymbolVal "S") Term
                               (Single (someSymbolVal "D") (Product Term))))

-- Case expansion --------------------------------------------------------------

caseExpansion ∷ ( HasState "constructors" (Map SomeSymbol Bound)    m
                , HasState "adtMap"       (Map SomeSymbol Branches) m
                , HasThrow "err"          Errors                    m
                , HasWriter "missingCases" [SomeSymbol]             m )
             ⇒ Switch → m Lambda
caseExpansion (Case _ []) = throw @"err" MatchWithoutCases
caseExpansion (Case on cases@(C c _ _:_)) = do
  cons ← get @"constructors"
  case cons Map.!? c of
    Nothing              → throw @"err" (NotInAdt c)
    Just Bound {adtName} → do
      adtConstructors ← getOrderedConstructors adtName
      -- Could be done faster by doing monadic recursion!
      case adtConstructors of
        [] → throw @"err" InvalidAdt
        _ → do
          let gen c accLam =
                Lambda (someSymbolVal "c%gen")
                  (Application (Application (Value (someSymbolVal "c%gen")) c)
                                accLam)
              lambdaFromEnv f t =
                case caseMap Map.!? t of
                  Nothing → do
                    tell @"missingCases" [t]
                    pure (f idL)
                  Just ([], body)   → pure (f (Lambda (someSymbolVal "()") body))
                  Just (args, body) → pure (f (foldr Lambda body args))

              recCase t accLam = lambdaFromEnv (flip gen accLam) t
              initial t        = lambdaFromEnv identity          t
              butLastadtCon    = reverse (tailSafe (reverse adtConstructors))
          last         ← initial (lastDef (error "doesn't happen") adtConstructors)
          expandedCase ← foldrM recCase last butLastadtCon
          return $ Application on
                 $ Lambda (someSymbolVal "rec")
                 $ expandedCase
  where
    caseMap = foldr (\ (C s args body) → Map.insert s (args, body)) mempty cases

    getOrderedConstructors adtName = do
      adtMap ← get @"adtMap"
      case adtMap Map.!? adtName of
        Just x  → return x
        Nothing → throw @"err" (AdtNotDefined adtName)

-- TODO ∷ replace recursive calls in the body with rec
-- TODO ∷ check if any value being matched on isn't called and throw an error

-- Lambda Abstraction for mendler encoding -------------------------------------

idL ∷ Lambda
idL = Lambda (someSymbolVal "x") (Value (someSymbolVal "x"))

inl :: Lambda
inl = Lambda x
           $ Lambda k
                  $ Lambda l
                         $ Application (Value k) (Value x)
      where
        x = someSymbolVal "x"
        k = someSymbolVal "k"
        l = someSymbolVal "l"

-- | Op of inl that has the first argument call the 2nd
-- useful for when constructing multiple argument passthrough
inlOp ∷ Lambda
inlOp = Lambda x
           $ Lambda k
                  $ Lambda l
                         $ Application (Value x) (Value k)
      where
        x = someSymbolVal "x"
        k = someSymbolVal "k"
        l = someSymbolVal "l"


inr ∷ Lambda
inr = Lambda y
           $ Lambda k
                  $ Lambda l
                         $ Application (Value l) (Value y)
      where
        y = someSymbolVal "y"
        k = someSymbolVal "k"
        l = someSymbolVal "l"

-- | Op of inr that has the first argument call the 2nd
-- useful for when constructing multiple argument passthrough
inrOp ∷ Lambda
inrOp = Lambda y
           $ Lambda k
                  $ Lambda l
                         $ Application (Value y) (Value l)
      where
        y = someSymbolVal "y"
        k = someSymbolVal "k"
        l = someSymbolVal "l"

foldM' ∷ Lambda
foldM' = Lambda alg $ Lambda d $ Application (Value d) (Value alg)
  where
    alg = someSymbolVal "alg"
    d   = someSymbolVal "d"

app ∷ Lambda → Lambda → Lambda
app (Lambda s t) replace = rec' t
  where
    rec' (Value s')
      | s == s'   = replace
      | otherwise = Value s'
    rec' (Lambda s' t)
      | s == s'   = Lambda s' t
      | otherwise = Lambda s' (rec' t)
    rec' (Application t1 t2) = Application (rec' t1) (rec' t2)
app t _replace = t

in' ∷ Lambda
in' = Lambda r
    $ Lambda f
    $ Application
      (Application (Value f)
                   (app foldM' (Value f)))
      (Value r)
  where
    r = someSymbolVal "r"
    f = someSymbolVal "f"

-- Test cases for Nat ----------------------------------------------------------
zero' :: Lambda
zero' = app in' (app inl (Lambda (someSymbolVal "x")
                           (Value (someSymbolVal "x"))))

succ' :: Lambda
succ' = Lambda (someSymbolVal "c%gen1")
               (app in' (app inr (app inl (Value (someSymbolVal "c%gen1")))))

dup' :: Lambda
dup' = Lambda (someSymbolVal "c%gen1")
       (Lambda (someSymbolVal "c%gen2")
         (app in' (app inr
                    (app inrOp (Lambda (someSymbolVal "%fun")
                                 (Application
                                   (Application (Value $ someSymbolVal "%fun")
                                                (Value $ someSymbolVal "c%gen1"))
                                   (Value $ someSymbolVal "c%gen2")))))))

test2D :: Either Errors (Lambda, Env)
test2D = runEnvsS $ do
  adtToMendler dUserNat
  caseExpansion (Case (Value $ someSymbolVal "val")
                  [ C (someSymbolVal "Z") []
                      (Value $ someSymbolVal "True")
                  , C (someSymbolVal "S") [someSymbolVal "n"]
                      (Application (Value $ someSymbolVal "not")
                                   (Application (Value $ someSymbolVal "rec")
                                                (Value $ someSymbolVal "n")))
                  , C (someSymbolVal "D") [someSymbolVal "n1"
                                          , someSymbolVal "n2"]
                      (Application (Value $ someSymbolVal "not")
                                   (Application (Value $ someSymbolVal "rec")
                                                (Value $ someSymbolVal "n1")))
                  ])



-- let rec f x i =
--   case x of
--   | Z       -> i
--   | S n     -> 1 + (f n i)
--   | D n1 n2 -> f n2 0 + f n1 i

test3D :: Either Errors (Lambda, Env)
test3D = runEnvsS $ do
  adtToMendler dUserNat
  caseExpansion (Case (Value $ someSymbolVal "val")
                  [ C (someSymbolVal "Z") []
                      (Value $ someSymbolVal "i")
                  , C (someSymbolVal "S") [someSymbolVal "n"]
                      (Application (Application (Value $ someSymbolVal "+")
                                                (Value $ someSymbolVal "1"))
                                   (Application (Application (Value $ someSymbolVal "rec")
                                                             (Value $ someSymbolVal "n"))
                                                (Value $ someSymbolVal "i")))
                  , C (someSymbolVal "D") [someSymbolVal "n1"
                                          , someSymbolVal "n2"]
                      (Application (Application (Value $ someSymbolVal "+")
                                                (Application (Application (Value $ someSymbolVal "rec")
                                                                          (Value $ someSymbolVal "n2"))
                                                             (Value $ someSymbolVal "0")))
                                   (Application (Application (Value $ someSymbolVal "rec")
                                                             (Value $ someSymbolVal "n1"))
                                                (Value $ someSymbolVal "i")))
                  ])

test1 :: Either Errors (Lambda, Env)
test1 = runEnvsS $ adtToMendler userNat >>
  caseExpansion (Case (Value $ someSymbolVal "val")
                  [ C (someSymbolVal "Z") []
                      (Value $ someSymbolVal "True")
                  , C (someSymbolVal "S") [someSymbolVal "n"]
                      (Application (Value $ someSymbolVal "not")
                                   (Application (Value $ someSymbolVal "rec")
                                                (Value $ someSymbolVal "n")))])
