-- Example ERC20 token contract in Idris.
import Data.SortedMap
import FakeLib

||| Account contains the balance and allowance of an associated address.
record Account where
  constructor MkAccount
  balance : Nat
  allowances : SortedMap Address Nat

||| The storage has type Storage which is a record with fields accounts,
||| version number of the token standard, total supply, name, symbol, and owner of tokens.
record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    version : Nat --version of the token standard
    totalSup : Nat
    name : String
    symbol : String
    owner : Address

initStorage : Storage
initStorage =
  MkStorage (insert "qwer" (MkAccount 1000 empty) empty) 1 1000 "Cool" "C" "qwer"

||| storage is the current storage
||| (for the totalSupply, balanceOf and allowance functions).
storage : Storage
storage =
  MkStorage (insert "qwer" (MkAccount 1000 empty) empty) 1 1000 "Cool" "C" "qwer"

||| getAccount returns the account of an associated key hash.
||| @address the key hash of the owner of the account
total getAccount : (address : Address) -> SortedMap Address Account -> Account
getAccount address accounts = case lookup address accounts of
                      Nothing => MkAccount 0 empty
                      (Just account) => account

total getAccountBalance : (address : Address) -> SortedMap Address Account -> Nat
getAccountBalance address accounts = case lookup address accounts of
                      Nothing => 0
                      (Just account) => balance account

total getAccountAllowance :
(address : Address) -> SortedMap Address Account -> SortedMap Address Nat
getAccountAllowance address accounts = case lookup address accounts of
                      Nothing => empty
                      (Just account) => allowances account

total modifyBalance :
(address : Address) -> (tokens : Nat) -> SortedMap Address Account ->
SortedMap Address Account
modifyBalance address tokens accounts =
  insert address (MkAccount tokens (getAccountAllowance address accounts)) accounts

-- Note: Nested record field update syntax(which is safer) doesn't seem to work
||| updateAllowance updates the allowance map of the allower
||| @allower the address of the owner of the allowance map that is to be updated
||| @allowee the address of the allowance from the allower that is to be updated
||| @tokens the amount of allowance approved by the allower to transfer to the allowee
||| @storage the current storage
total updateAllowance :
(allower : Address) -> (allowee : Address) -> (tokens : Nat)
-> (storage : Storage) -> Storage
updateAllowance allower allowee tokens storage =
  let allowerBal = getAccountBalance allower (accounts storage)
      allowerMap = getAccountAllowance allower (accounts storage) in
    case tokens of
      Z => record
             {accounts =
                insert -- update allower's allowance map
                allower -- k
                (MkAccount -- v
                  allowerBal -- balance of allower is unchanged
                  (delete allowee allowerMap) -- delete allowee from allowance map
                )
                (accounts storage) -- update the accounts map
             } storage
      n => record
             {accounts =
                insert
                allower -- k
                (MkAccount -- v
                  allowerBal
                  (insert allowee n allowerMap)
                )
                (accounts storage) -- accounts map
             } storage

-- contract entry points/functions as per the ERC20 standard:

total totalSupply : Nat
totalSupply = totalSup storage

total balanceOf : Address -> Nat
balanceOf owner = getAccountBalance owner (accounts storage)

total allowance : (owner : Address) -> (spender : Address) -> Nat
allowance owner spender =
  case lookup spender (getAccountAllowance owner (accounts storage)) of
    Nothing => Z
    (Just n) => n

||| performTransfer transfers tokens from the from address to the dest address.
||| @from the address the tokens to be transferred from
||| @dest the address the tokens to be transferred to
||| @tokens the amount of tokens to be transferred
||| @storage the current storage
total performTransfer :
(from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage)
-> Either Error Storage
performTransfer from dest tokens storage =
  let fromBalance = getAccountBalance from (accounts storage)
      destBalance = getAccountBalance dest (accounts storage) in
        case lte tokens fromBalance of
             False => Left NotEnoughBalance
             True =>
               let accountsStored =
                 modifyBalance from (minus fromBalance tokens) (accounts storage) in
                 Right
                   (record
                     {accounts =
                       modifyBalance dest (destBalance + tokens) accountsStored
                     } storage)

||| approve update the allowance map of the caller of the contract
||| @spender the address the caller approve the tokens to transfer to
||| @tokens the amount of tokens approved to be transferred
||| @storage the current storage
total approve :
(spender : Address) -> (tokens : Nat) -> (storage : Storage) -> Storage
approve spender tokens storage =
    updateAllowance currentCaller spender tokens storage

||| transferFrom can be called by anyone,
||| transferring amount no larger than the approved amount
||| @from the address the tokens to be transferred from
||| @dest the address the tokens to transfer to
||| @tokens the amount to be transferred
||| @storage the current storage
total transferFrom :
(from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage)
-> Either Error Storage
transferFrom from dest tokens storage =
  case lookup dest (getAccountAllowance from (accounts storage)) of
    Nothing => Left NotAllowedToSpendFrom
    (Just allowed) =>
      case lte tokens allowed of
        False => Left NotEnoughAllowance
        True =>
          let updatedStorage = updateAllowance from dest (minus allowed tokens) storage in
                    performTransfer from dest tokens updatedStorage

||| createAccount transfers tokens from the owner to an address
||| @dest the address of the account to be created
||| @tokens the amount of tokens in the new created account
||| @storage the current storage
total createAccount :
(dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error Storage
createAccount dest tokens storage =
  let owner = owner storage in
      case currentCaller == owner of
           False => Left FailedToAuthenticate
           True => performTransfer owner dest tokens storage

-- entry points/functions that are NOT in the ERC20 standard

||| burn the caller of this function destroys/burns their own tokens
||| @tokens the amount of tokens to destory
||| @storage the original storage
total burn : (tokens : Nat) -> (storage : Storage) -> Either Error Storage
burn tokens storage =
  let burnerBal = getAccountBalance currentCaller (accounts storage)
      updatedStorage = record {totalSup = minus (totalSup storage) tokens} storage in
      case lte tokens burnerBal of
        False => Left NotEnoughBalance
        True =>
          Right
            (record
              {accounts =
                modifyBalance
                  currentCaller
                  (minus burnerBal tokens)
                  (accounts storage)
              } updatedStorage
            )

||| mint when the caller is the owner of the token contract,
||| they can mint/add new tokens to the total supply
||| @tokens the amount of tokens to add to the total supply
||| @dest the address the new tokens to be added to
||| @storage the original storage
total mint :
(tokens : Nat) -> (dest : Address) -> (storage : Storage) -> Either Error Storage
mint tokens dest storage =
  let destBal = getAccountBalance dest (accounts storage)
      updatedStorage = record {totalSup = (totalSup storage) + tokens} storage in
      case currentCaller == owner storage of
        False => Left FailedToAuthenticate
        True =>
          Right
            (record
              {accounts =
                modifyBalance
                  dest
                  (destBal + tokens)
                  (accounts storage)
              } updatedStorage
            )
