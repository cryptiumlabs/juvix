{-# OPTIONS_GHC -Wall #-}
--Dependent type implementation following
--"A tutorial implementation of a dependently typed lambda calculus"

import           Control.Monad.Except

-- Inferable terms
data ITerm
  =  Ann    CTerm CTerm --annotated terms
  |  Star --(STAR) Star (the type of types) is now a term
  |  Pi CTerm CTerm --(PI) dependent function space
  |  Bound  Int --Bound variables of type Int because it's represented by de Bruijn indices
  |  Free   Name --Free variables of type name (see below)
  |  ITerm :@: CTerm --the infix constructor :@: denotes application
  |  Nat --Nat data type
  |  Zero --data constructor of Nat
  |  Succ CTerm --data constructor of Nat
  |  NatElim CTerm CTerm CTerm CTerm --eliminator of Nat
  |  Vec CTerm CTerm --Vec (vector) data type
  |  Nil CTerm --data constructor of Vec
  |  Cons CTerm CTerm CTerm CTerm --data constructor of Vec
  |  VecElim CTerm CTerm CTerm CTerm CTerm CTerm --eliminator of Vec
  |  Eq CTerm CTerm CTerm --Eq (equality) data type
  |  Refl CTerm CTerm --data constructor of Eq
  |  EqElim CTerm CTerm CTerm CTerm CTerm CTerm --eliminator of Eq
  deriving (Show, Eq)

--Checkable terms
data CTerm
  =  Inf  ITerm --(CHK) Inf is the constructor that embeds ITerm to CTerm
  |  Lam  CTerm --(LAM) Lam stands for Lambda abstractions
  deriving (Show, Eq)

--Helper functions for turning ITerms to CTerm
cStar :: CTerm
cStar = Inf Star
cPi :: CTerm -> CTerm -> CTerm
cPi f t = Inf (Pi f t)
cBound :: Int -> CTerm
cBound x = Inf (Bound x)
cNat :: CTerm
cNat = Inf Nat
cZero :: CTerm
cZero = Inf Zero
cSucc :: CTerm -> CTerm
cSucc k = Inf (Succ k)
cNatElim :: CTerm -> CTerm -> CTerm -> CTerm -> CTerm
cNatElim m mz ms k = Inf (NatElim m mz ms k)
cEq :: CTerm -> CTerm -> CTerm -> CTerm
cEq a x y = Inf (Eq a x y)
cRefl :: CTerm -> CTerm -> CTerm
cRefl a x = Inf (Refl a x)
cEqElim :: CTerm -> CTerm -> CTerm -> CTerm -> CTerm -> CTerm -> CTerm
cEqElim a m mr x y e = Inf (EqElim a m mr x y e)

data Name
  =  Global  String --Global variables are represented by name thus type string
  |  Local   Int --to convert a bound variable into a free one
  |  Quote   Int
  deriving (Show, Eq)

--Values can now also be VStar or VPi
data Value
  = VLam (Value -> Value)
  | VStar
  | VPi Value (Value -> Value)
  | VNeutral Neutral
  | VNat --extensions for Nat
  | VZero
  | VSucc Value
  | VNil Value --extensions for Vec
  | VCons Value Value Value Value
  | VVec Value Value
  | VRefl Value Value --extensions for Eq
  | VEq Value Value Value

showVal :: Value -> String
showVal (VLam _)         = "lambda"
showVal (VStar)          = "*"
showVal (VPi v _f)       = "pi " ++ showVal v ++ " -> lambda"
showVal (VNeutral _n)    = "neutral "
showVal (VNat)           = "Nat "
showVal (VZero)          = "Z "
showVal (VSucc v)        = "(S " ++ " " ++ showVal v ++ ")"
showVal (VNil t)         = "Vec " ++ showVal t ++ " 0"
showVal (VCons a k x xs) = "Cons " ++ foldl (++) [] (map showVal [a, k, x, xs])
showVal (VVec t l)       = "Vec " ++ showVal t ++ " Length " ++ showVal l
showVal (VRefl x y)      = "Refl " ++ showVal x ++ showVal y
showVal (VEq a x y)      = "Eq " ++ foldl (++) [] (map showVal [a, x, y])

--A neutral term is either a variable or an application of a neutral term to a value
data Neutral
  = NFree Name
  | NApp Neutral Value
  | NNatElim Value Value Value Neutral --for NatElim to not be stuck
  | NVecElim Value Value Value Value Value Neutral --for VecElim
  | NEqElim Value Value Value Value Value Neutral --for EqElim

--vfree creates the value corresponding to a free variable
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

--Contexts map variables to their types.
type Type    = Value
type Context = [(Name, Type)]

--Evaluation
type Env = [Value]
iEval :: ITerm -> Env -> Value
iEval (Ann  e _)    d  =  cEval e d
iEval (Free  x)    _d  =  vfree x
iEval (Bound  ii)   d  =  d !! ii --(!!) :: [a] -> Int -> a. It's the list lookup operator.
iEval (e1 :@: e2)   d  =  vapp (iEval e1 d) (cEval e2 d)
iEval Star         _d  =  VStar
iEval (Pi ty ty')   d  =  VPi (cEval ty d)(\ x -> cEval ty' (x : d))
--evaluation of Nat
iEval Nat                 _d  =  VNat
iEval Zero                _d  =  VZero
iEval (Succ k)             d  =  VSucc (cEval k d)
iEval (NatElim m mz ms k)  d
  = let mzVal = cEval mz d
        msVal = cEval ms d
        rec kVal =
          case kVal of
            VZero      -> mzVal
            VSucc l    -> msVal `vapp` l `vapp` rec l
            VNeutral n -> VNeutral
                          (NNatElim (cEval m d) mzVal msVal n)
            term       -> error ("internal: eval natElim. \n" ++ showVal term ++ "\n cannot be evaluated.")
    in rec (cEval k d)
--evaluation of Vec
iEval (Nil a)                   d  =  VNil (cEval a d)
iEval (Cons a k x xs)           d  =  VCons  (cEval a d) (cEval k d)
                                      (cEval x d) (cEval xs d)
iEval (Vec a k)                 d  =  VVec (cEval a d) (cEval k d)
iEval (VecElim a m mn mc k xs)  d  =
    let mnVal  =  cEval mn d
        mcVal  =  cEval mc d
        rec kVal xsVal =
          case xsVal of
            VNil _           ->  mnVal
            VCons _ l x xs_  ->  foldl vapp mcVal [l, x, xs_, rec l xs_]
            VNeutral n       ->  VNeutral
                                 (NVecElim  (cEval a d) (cEval m d)
                                            mnVal mcVal kVal n)
            term             ->  
              error ("internal: eval vecElim. \n" ++ showVal term ++ " \n cannot be evaluated.")
    in rec (cEval k d) (cEval xs d)
--evaluation of Eq
iEval (Refl x y)                d  =  VRefl (cEval x d) (cEval y d)
iEval (Eq a x y)                d  =  VEq (cEval a d) (cEval x d) (cEval y d)
iEval (EqElim a m mr x y eq)    d  =
  let mrVal  =  cEval mr d
      recu eqVal =
        case eqVal of
          VRefl _ z  -> mrVal `vapp` z
          VNeutral n ->
            VNeutral (NEqElim  (cEval a d) (cEval m d) mrVal
                                (cEval x d) (cEval y d) n)
          term       -> 
            error ("internal: eval eqElim. \n " ++ showVal term ++ "\n cannot be evaluated.")
  in recu (cEval eq d)

vapp :: Value -> Value -> Value
vapp (VLam f)      v =  f v
vapp (VNeutral n)  v =  VNeutral (NApp n v)
vapp x             y =  error ("Cannot apply \n" ++ showVal y ++ "\n to \n" ++ showVal x)

cEval :: CTerm -> Env -> Value
cEval (Inf  ii)   d =  iEval ii d
cEval (Lam  e)    d =  VLam (\ x -> cEval e (x : d))

--Substitution

--substitution function for inferable terms
iSubst :: Int -> ITerm -> ITerm -> ITerm
iSubst ii r (Ann e ty)     =  Ann (cSubst ii r e) (cSubst ii r ty)
iSubst ii r (Bound j)      =  if ii == j then r else Bound j
iSubst _ii _r (Free y)     =  Free y
iSubst ii r (e1 :@: e2)    =  iSubst ii r e1 :@: cSubst ii r e2
iSubst _ii _r Star         =  Star
iSubst ii r (Pi ty ty')    =  Pi  (cSubst ii r ty) (cSubst (ii + 1) r ty')
--for Nat
iSubst _ii _r Nat          =  Nat
iSubst _ii _r Zero         =  Zero
iSubst ii r (Succ n)       =  Succ (cSubst ii r n)
iSubst ii r (NatElim m mz ms n)
                           =  NatElim (cSubst ii r m)
                                      (cSubst ii r mz) (cSubst ii r ms)
                                      (cSubst ii r n)
--for Vec
iSubst ii r  (Vec a n)     =  Vec (cSubst ii r a) (cSubst ii r n)
iSubst ii r  (Nil a)       =  Nil (cSubst ii r a)
iSubst ii r  (Cons a n x xs)
                           =  Cons (cSubst ii r a) (cSubst ii r n)
                                     (cSubst ii r x) (cSubst ii r xs)
iSubst ii r  (VecElim a m mn mc n xs)
                           =  VecElim (cSubst ii r a) (cSubst ii r m)
                                        (cSubst ii r mn) (cSubst ii r mc)
                                        (cSubst ii r n) (cSubst ii r xs)
--for Eq
iSubst ii r  (Eq a x y)    =  Eq (cSubst ii r a) (cSubst ii r x) (cSubst ii r y)
iSubst ii r  (Refl x y)    =  Refl (cSubst ii r x) (cSubst ii r y)
iSubst ii r  (EqElim a m mr x y eq)
                           =  EqElim (cSubst ii r a) (cSubst ii r m)
                                     (cSubst ii r mr) (cSubst ii r x)
                                     (cSubst ii r y) (cSubst ii r eq)

--substitution function for checkable terms
cSubst :: Int -> ITerm -> CTerm -> CTerm
cSubst ii r (Inf e) =  Inf (iSubst ii r e)
cSubst ii r (Lam e) =  Lam (cSubst (ii + 1) r e)

--Quotation: takes a value back to a term
quote0 :: Value -> CTerm
quote0 = quote 0

quote :: Int -> Value -> CTerm
quote ii (VLam f)      =  Lam (quote (ii + 1) (f (vfree (Quote ii))))
quote ii (VNeutral n)  =  Inf (neutralQuote ii n)
quote _ii VStar        =  cStar
quote ii (VPi v f)     =  cPi (quote ii v)(quote (ii + 1)(f (vfree(Quote ii))))
--for Nat
quote _ii VNat         =  cNat
quote _ii VZero        =  cZero
quote ii (VSucc n)     =  cSucc (quote ii n)
--for Vec
quote ii (VVec a n)         =  Inf (Vec (quote ii a) (quote ii n))
quote ii (VNil a)           =  Inf (Nil (quote ii a))
quote ii (VCons a n x xs)   =  Inf (Cons  (quote ii a) (quote ii n)
                                    (quote ii x) (quote ii xs))
--for Eq
quote ii (VEq a x y)        =  cEq (quote ii a) (quote ii x) (quote ii y)
quote ii (VRefl x y)        =  cRefl (quote ii x) (quote ii y)

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote ii (NFree x)   =  boundfree ii x
neutralQuote ii (NApp n v)  =  neutralQuote ii n :@: quote ii v
neutralQuote ii (NNatElim m z s n)
  =  NatElim (quote ii m) (quote ii z) (quote ii s) (Inf (neutralQuote ii n))
neutralQuote ii (NVecElim a m mn mc n xs)
  =  VecElim (quote ii a) (quote ii m)
               (quote ii mn) (quote ii mc)
               (quote ii n) (Inf (neutralQuote ii xs))
neutralQuote ii (NEqElim a m mr x y eq)
  =  EqElim (quote ii a) (quote ii m) (quote ii mr)
                 (quote ii x) (quote ii y)
                 (Inf (neutralQuote ii eq))
--checks if the variable occurring at the head of the application is a bound variable or a free name
boundfree :: Int -> Name -> ITerm
boundfree ii (Quote k) =  Bound (ii - k - 1)
boundfree _ii x        =  Free x

--Type checking

type Result a = Either String a --when type checking fails, it throws an error.

--inferable terms has type as output.
iType0 :: Context -> ITerm -> Result Type
iType0 = iType 0

iType :: Int -> Context -> ITerm -> Result Type
iType ii g (Ann e rho)
  =  do cType ii g rho VStar
        let ty = cEval rho []
        cType ii g e ty
        return ty
iType _ii _g Star
  =  return VStar
iType ii g (Pi rho rho') 
  =  do cType ii g rho VStar
        let ty = cEval rho []
        cType (ii + 1) ((Local ii, ty): g)
              (cSubst 0 (Free (Local ii))rho') VStar
        return VStar
iType ii g (Free x)
  =  case lookup x g of
        Just ty ->  return ty
        Nothing ->  
          throwError ("Cannot find the type of \n" ++ show x ++ 
          "\n (binder number " ++show ii++ ") in the environment.")
iType ii g (e1 :@: e2)
  = do si <- iType ii g e1
       case si of
         VPi ty ty'  ->  do  cType ii g e2 ty
                             return (ty' (cEval e2 []))
         _           ->  
          throwError (show e1 ++ "\n (binder number " ++show ii++ 
          ") is not a function type and thus \n" 
          ++ show e1 ++ "\n cannot be applied to it.")
--type checker for Nat
iType _ii _g Nat                =  return VStar
iType _ii _g Zero               =  return VNat
iType ii g (Succ k)             =
  do cType ii g k VNat
     return VNat
iType ii g (NatElim m mz ms k)  =
  do cType ii g m (VPi VNat (const VStar))
     let mVal = cEval m []
     cType ii g mz (mVal `vapp` VZero)
     cType ii g ms (VPi VNat (\ l -> VPi (mVal `vapp` l) (\_ -> mVal `vapp` VSucc l)))
     cType ii g k VNat
     let kVal = cEval k []
     return (mVal `vapp` kVal)
--type checker for Vec
iType ii g (Vec a k) =
  do  cType ii g a VStar
      cType ii g k VNat
      return VStar
iType ii g (Nil a)   =
  do  cType ii g a VStar
      let aVal = cEval a []
      return (VVec aVal VZero)
iType ii g (Cons a k x xs)      =
  do  cType ii g a VStar
      let aVal = cEval a []
      cType ii g k VNat
      let kVal = cEval k []
      cType ii g x aVal
      cType ii g xs (VVec aVal kVal)
      return (VVec aVal (VSucc kVal))
iType ii g (VecElim a m mn mc k vs) =
  do  cType ii g a VStar
      let aVal = cEval a []
      cType ii g m
        (  VPi VNat (\k_ -> VPi (VVec aVal k_) (\ _ -> VStar)))
      let mVal = cEval m []
      cType ii g mn (foldl vapp mVal [VZero, VNil aVal])
      cType ii g mc
        (  VPi VNat (\ l ->
           VPi aVal (\ y ->
           VPi (VVec aVal l) (\ ys ->
           VPi (foldl vapp mVal [l, ys]) (\ _ ->
           (foldl vapp mVal [VSucc l, VCons aVal l y ys]))))))
      cType ii g k VNat
      let kVal = cEval k []
      cType ii g vs (VVec aVal kVal)
      let vsVal = cEval vs []
      return (foldl vapp mVal [kVal, vsVal])
--type checker for Eq
iType ii g (Eq a x y) =
  do  cType ii g a VStar
      let aVal = cEval a []
      cType ii g x aVal
      cType ii g y aVal
      return VStar
iType ii g (Refl x y)  =
  do  let xVal = cEval x []
          yVal = cEval y []
      cType ii g x VStar
      cType ii g y xVal
      return (VEq xVal yVal yVal)
iType ii g (EqElim a m mr x y eq) =
  do  cType ii g a VStar
      let aVal = cEval a []
      cType ii g m
        (VPi aVal (\ x_ ->
         VPi aVal (\ y_ ->
         VPi (VEq aVal x_ y_) (\ _ -> VStar))))
      let mVal = cEval m []
      cType ii g mr
        (VPi aVal (\ x_ ->
         foldl vapp mVal [x_, x_]))
      cType ii g x aVal
      let xVal = cEval x []
      cType ii g y aVal
      let yVal = cEval y []
      cType ii g eq (VEq aVal xVal yVal)
      return (foldl vapp mVal [xVal, yVal])
iType ii _g iterm                     =  
  throwError (show iterm ++ "\n (binder number " ++ show ii ++ ") is not an inferable term.")

--error message for inferring/checking types
errorMsg :: Int -> ITerm -> Value -> Value -> String
errorMsg binder iterm expectedT gotT = 
  "Type mismatched. \n" ++ show iterm ++ " \n (binder number " ++show binder++ ") is of type \n" ++ show (showVal gotT) ++ 
  "\n but the expected type is " ++ show (showVal expectedT) ++ "." 

--checkable terms takes a type as input and returns ().
cType :: Int -> Context -> CTerm -> Type -> Result ()
cType ii g (Inf e) v
  =  do v' <- iType ii g e
        unless (quote0 v == quote0 v') (throwError (errorMsg ii e v v')) 
cType ii g (Lam e) (VPi ty ty')
  =  cType (ii + 1) ((Local ii, ty) : g)
           (cSubst 0 (Free (Local ii)) e) (ty' (vfree (Local ii)))
cType ii _g cterm theType 
  =  throwError 
       ("Type mismatch: \n" ++ show cterm ++ 
       "\n (binder number " ++ show ii ++ 
       ") is not a checkable term. Cannot check that it is of type " 
       ++ showVal theType)

--motive for plusK
motivePlus :: CTerm
motivePlus = Lam (cPi cNat cNat)
--following paper, plus add two Nats. plus :: Nat -> Nat -> Nat.
plus :: CTerm -> ITerm
plus = NatElim
  motivePlus -- motive
  (Lam (cBound 0)) --(\n -> n)
  --(\k \rec \n -> Succ(rec n)), where rec is adding k
  (Lam (Lam (Lam (cSucc (Inf ((Bound 1) :@: (cBound 0)))))))
--plusZero adds zero to a Nat. plusZero :: Nat -> Nat.  
plusZero :: ITerm
plusZero = plus cZero

plusOne :: ITerm
plusOne = plus (cSucc cZero)

plusTwo :: ITerm
plusTwo = plus (cSucc (cSucc cZero))
plusZeroIsIdentityIMType :: CTerm
plusZeroIsIdentityIMType =
  cPi cNat 
    (cPi cNat
      (cPi (cEq cNat (cBound 1) (cBound 2))
      cStar))
--Turn plusZeroIsIdentityIM from a CTerm to an ITerm.
iplusZeroIsIdentityIM :: ITerm
iplusZeroIsIdentityIM =
  Ann plusZeroIsIdentityIMType plusZeroIsIdentityIM
--the motive of the EqElim for plusZeroIsIdentityInductive
plusZeroIsIdentityIM :: CTerm
plusZeroIsIdentityIM = 
  (Lam --x::Nat, in this case plus x 0
    (Lam --y::Nat, in this case x
      (Lam --of type Eq a x y
        --output has to be of type type, 
        --in this case Eq Nat (plus (Succ x) 0) (Succ x)       
        (cEq cNat 
             (Inf (plusZero :@: (cSucc (cBound 1)))) 
             (cSucc (cBound 1))
        )
      )
    )
  )

--The function to prove inductive case of plusZeroIsIdentity.
--plusZeroIsIdentityInductive :: 
--For all k::Nat.Eq Nat (plusZero k) k -> Eq Nat (plusZero (Succ k)) (Succ k)
plusZeroIsIdentityInductive :: CTerm
plusZeroIsIdentityInductive = 
  (Lam  --k :: Nat
    (cEqElim
      --1st argument: of type type, in this case Nat.
      cNat
      --2nd argument: motive, takes in x, y, Eq a x y and returns a type,
      --in this case it's Eq Nat (plusZero (Succ k)) (Succ k) 
      plusZeroIsIdentityIM
      --3rd argument, resulting type should be 
      --for all z::Nat.Eq Nat (plusZero (Succ z)) (Succ z).
      (Lam (cRefl cNat (cSucc (cBound 0))))
      --4th argument, x, of type Nat
      (Inf (plusZero :@: (cBound 0)))
      --5th argument, y, of type Nat
      (cBound 0)
      --6th argument, of type Eq a x y
      (cRefl cNat (cBound 0))
      )
    )
--Proof of x + 0 = x
plusZeroIsIdentity :: CTerm -> ITerm
plusZeroIsIdentity = NatElim --point-free style, x is omitted
  --motive takes in x and returns the type Eq Nat (x+0) x, \x. Eq Nat (x + 0) x
  (Lam (cEq cNat (Inf (plusZero :@: (cBound 0))) (cBound 0)))
  (cRefl cNat cZero) --m Zero, with type Eq Nat 0 0.
  --inductive case, the result have to be of type Eq Nat (k + 1 + 0) (k + 1) 
  plusZeroIsIdentityInductive

--Proof of x + y = y + x

--the motive of the EqElim for plusCommInductive
plusCommIM :: CTerm
plusCommIM = 
  (Lam 
    (Lam 
      (Lam 
        (cEq cNat 
             (Inf (plus (cSucc (cBound 2)) :@: (cBound 1))) 
             (Inf ((plus (cBound 1)) :@: (cSucc (cBound 2))))
        )
      )
    )
  )
--The function to prove inductive case of plusComm.
--plusComm :: For all k,x::Nat, Eq Nat (plus k x) (plus x k) -> Eq Nat (plus (Succ k) x) (plus x (Succ k))
plusCommInductive :: CTerm
plusCommInductive =
  (Lam --k::Nat
    (Lam --x::Nat
      (Lam 
        (cEqElim
      --first argument of EqElim, of type type, in this case Nat
          (cBound 0)
          --2nd argument: motive, takes in x, y, Eq a x y and returns a type,
          --in this case it's Eq Nat (plus (Succ k) x) (plus x (Succ k)) 
          plusCommIM
          --3rd argument, resulting type should be Eq Nat (plus (Succ k) x) (plus x (Succ k)).
          (Lam (cRefl cNat (cSucc (cBound 2))))
          --4th argument, x, of type Nat
          (Inf ((plus (cBound 2)) :@: (cBound 1)))
          --5th argument, y, of type Nat
          (Inf (plus (cBound 1) :@: (cBound 2)))
          --6th argument, of type Eq a x y
          (cRefl cNat (Inf ((plus (cBound 1)) :@: (cBound 2))))
        )
      )
    )
  )

plusComm :: CTerm -> CTerm -> ITerm
plusComm x y = NatElim
  --motive takes in x and y and returns the type Eq Nat (plus x y) (plus y x)
  (Lam (Lam (cEq cNat (Inf (plus (cBound 1) :@: (cBound 0))) (Inf ((plus (cBound 0)) :@: (cBound 1))))))
  (cRefl cNat (Inf (plusZero :@: (cBound 1)))) --m Zero, x + 0 = 0 + x
  --inductive case, the result have to be of type Eq Nat (x + (k+1)) ((k+1) + x)
  plusCommInductive
  y
