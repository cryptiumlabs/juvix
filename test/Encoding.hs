import           Juvix.Library   hiding (Sum, Product)
import           Juvix.Encoding.Types
import           Juvix.Encoding.Encoding
import           Juvix.Encoding.Scott
import           Juvix.Encoding.Mendler



userNat :: Name
userNat = Adt (someSymbolVal "Nat")
              (Branch (someSymbolVal "Z") None
                      (Single (someSymbolVal "S") Term))

dUserNat ∷ Name
dUserNat = Adt (someSymbolVal "Nat")
               (Branch (someSymbolVal "Z") None
                       (Branch (someSymbolVal "S") Term
                               (Single (someSymbolVal "D") (Product Term))))


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
  mendlerCase (Case (Value $ someSymbolVal "val")
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
  mendlerCase (Case (Value $ someSymbolVal "val")
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

test3D' :: Either Errors (Lambda, Env)
test3D' = runEnvsS $ do
  adtToScott dUserNat
  scottCase (Case (Value $ someSymbolVal "val")
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
test1 = runEnvsS $ do
  adtToMendler userNat
  mendlerCase (Case (Value $ someSymbolVal "val")
                  [ C (someSymbolVal "Z") []
                      (Value $ someSymbolVal "True")
                  , C (someSymbolVal "S") [someSymbolVal "n"]
                      (Application (Value $ someSymbolVal "not")
                                   (Application (Value $ someSymbolVal "rec")
                                                (Value $ someSymbolVal "n")))])


test1' :: Either Errors (Lambda, Env)
test1' = runEnvsS $ do
  adtToScott userNat
  scottCase (Case (Value $ someSymbolVal "val")
                  [ C (someSymbolVal "Z") []
                      (Value $ someSymbolVal "True")
                  , C (someSymbolVal "S") [someSymbolVal "n"]
                      (Application (Value $ someSymbolVal "not")
                                   (Application (Value $ someSymbolVal "rec")
                                                (Value $ someSymbolVal "n")))])

