module Main

import Data.SortedMap
import Data.Vect

||| Account contains the balance of the token.
Account : Type
Account = Nat

||| Address is the key hash, a string of length 36.
Address : Type
Address = String

||| The storage has type Storage which is a record with fields accounts,
||| version number of the token standard, total supply, name, symbol, and owner of tokens.
record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    version : Nat --version of the token standard
    totalSupply : Nat
    name : String
    symbol : String
    owner : Address

data Error = NotEnoughBalance
           | FailedToAuthenticate
           | InvariantsDoNotHold

||| getAccount returns the balance of an associated key hash.
||| @address the key hash of the owner of the balance
total getAccount : (address : Address) -> SortedMap Address Account -> Nat
getAccount address accounts = case lookup address accounts of
                      Nothing => 0
                      (Just balance) => balance

||| performTransfer transfers tokens from the from address to the dest address.
||| @from the address the tokens to be transferred from
||| @dest the address the tokens to be transferred to
||| @tokens the amount of tokens to be transferred
total performTransfer : (from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error (Storage)
performTransfer from dest tokens storage =
  let fromBalance = getAccount from (accounts storage)
      destBalance = getAccount dest (accounts storage) in
        case lte tokens fromBalance of
             False => Left NotEnoughBalance
             True => let accountsStored = insert from (minus fromBalance tokens) (accounts storage) in
                       Right (record {accounts = (insert dest (destBalance + tokens) accountsStored)} storage)

||| transferInvariants makes sure the totalSupply is unchanged with performTransfer
transferInvariants : (from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Bool
transferInvariants from dest tokens storage =
  let transfer = performTransfer from dest tokens storage in
    case transfer of
      Left _ => False
      Right sAfterTransfer => totalSupply storage == totalSupply sAfterTransfer
{-
provenAction : (Storage -> Either Error Storage) -> Storage -> Either Error storage
provenAction fn storage =
  let result = fn storage in
  case result of
    Left -> result
    Right newStorage ->
      if invariants then result
      else Left InvariantsDoNotHold

provenAction (transfer f d t) s
provenAction (createAccount d t) s
-}
||| provenTransfer runs performTransfer if transferInvariants returns True.
total provenTransfer : (from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error Storage
provenTransfer from dest tokens storage =
  if transferInvariants (owner storage) dest tokens storage then
    performTransfer from dest tokens storage
  else
    Left InvariantsDoNotHold

||| createAccount transfers tokens from the owner to an address
||| @dest the address of the account to be created
||| @tokens the amount of tokens in the new created account
total createAccount : (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error Storage
createAccount dest tokens storage =
    let owner = owner storage in
      case owner == owner of --when sender can be detected, check sender == owner.
           False => Left FailedToAuthenticate
           True => performTransfer owner dest tokens storage
{-||| FName are the names of functions
data FName = PerformTransfer
           | CreateAccount

||| fnSig returns the function signature of the input function
total fnSig : FName -> Type
fnSig PerformTransfer = Either Error (Storage)
  --(from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error (Storage)
fnSig CreateAccount = Either Error (Either Error (Storage))
  --(dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error (Either Error (Storage))


--||| totalSupplyInvariant makes sure the totalSupply is unchanged with all relevant functions
totalSupplyInvariant : (Storage -> Either Error Storage) -> Bool
totalSupplyInvariant fn =
  case (fn of
      Left _ => False
      Right afterfn => totalSupply storage == totalSupply afterfn
-}
