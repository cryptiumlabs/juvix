module Main

import Data.SortedMap
import Data.Vect
import Data.Fin

||| Account contains the balance of the token.
Account : Type
Account = Nat

||| Address is the key hash, a string of length 36.
Address : Type
Address = String

sumOfAccounts : SortedMap Address Account -> Nat
sumOfAccounts accounts = sum $ values accounts

||| The storage has type Storage which is a record with fields accounts,
||| version number of the token standard, total supply, name, symbol, and owner of tokens.
record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    version : Nat --version of the token standard
    totalSupply : Fin (sumOfAccounts accounts)
    name : String
    symbol : String
    owner : Address

--use setter function to set owner?
data Error = NotEnoughBalance
           | FailedToAuthenticate
           | InvariantsDoNotHold

initStorage : Storage
initStorage =
  MkStorage (insert "qwer" 1000 empty) 1 1000 "Cool" "C" "qwer"

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

||| createAccount transfers tokens from the owner to an address
||| @dest the address of the account to be created
||| @tokens the amount of tokens in the new created account
total createAccount : (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error Storage
createAccount dest tokens storage =
    let owner = owner storage in
      case owner == owner of --when sender can be detected, check sender == owner.
           False => Left FailedToAuthenticate
           True => performTransfer owner dest tokens storage
-- **********End of contract without safety checks**********

-- Below code is optional,
-- running above functions (e.g., performTransfer) via provenAction adds safety listed in invariants.
||| invariants checks whether certain invariants hold and returns True if they do.
||| @oldStorage the storage before running the function
||| @newStorage the storage after running the function
invariants : (oldStorage : Storage) -> (newStorage : Storage) -> Bool
invariants oldS newS =
 (totalSupply oldS == totalSupply newS) && --totalSupply conserved
 (owner oldS == owner newS) --the owner of the token contract is unchanged

||| provenTotalSupplyAction checks that totalSupply is conserved from running the input function.
||| @fn the input function (e.g., performTransfer) and all its input arguments except the storage
||| @storage the storage input to fn
provenAction : (fn : (Storage -> Either Error Storage)) -> (storage : Storage) -> Either Error Storage
provenAction fn storage =
 let result = fn storage in
 case result of
   Left _ => result
   Right newStorage =>
     if invariants storage newStorage then result
     else Left InvariantsDoNotHold
