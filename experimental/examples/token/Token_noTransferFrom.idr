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

transferInvariants : (from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Bool
transferInvariants from dest tokens storage =
  let transfer = performTransfer from dest tokens storage in
    case transfer of
      Left _ => False
      Right sAfterTransfer => totalSupply storage == totalSupply sAfterTransfer

||| createAccount transfers tokens from the owner to an address
||| @dest the address of the account to be created
||| @tokens the amount of tokens in the new created account
total createAccount : (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error (Either Error (Storage))
createAccount dest tokens storage =
  if transferInvariants (owner storage) dest tokens storage then --can include other invariants that need to hold
    let owner = owner storage in
      case owner == owner of --when sender can be detected, check sender == owner.
           False => Left FailedToAuthenticate
           True => Right (performTransfer owner dest tokens storage)
  else
    Left InvariantsDoNotHold --if any of the variants do not hold, createAccount will not go through. So if it goes through the invariants hold.
