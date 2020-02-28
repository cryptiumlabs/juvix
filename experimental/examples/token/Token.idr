-- Example ERC20 token contract in Idris.
import Data.SortedMap

||| Address is the key hash of the owner of the associated account.
Address : Type
Address = String

||| Account contains the balance and allowance of an associated address.
record Account where
  constructor MkAccount
  balance : Nat
  allowance : SortedMap Address Nat

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

||| getAccount returns the balance of an associated key hash.
||| @address the key hash of the owner of the balance
total getAccount : (address : Address) -> SortedMap Address Account -> Account
getAccount address accounts = case lookup address accounts of
                      Nothing => MkAccount 0 empty
                      (Just account) => account

total getAccountBalance : (address : Address) -> SortedMap Address Account -> Nat
getAccountBalance address accounts = case lookup address accounts of
                      Nothing => 0
                      (Just account) => balance account

total getAccountAllowance : (address : Address) -> SortedMap Address Account -> SortedMap Address Nat
getAccountAllowance address accounts = case lookup address accounts of
                      Nothing => empty
                      (Just account) => allowance account

||| performTransfer transfers tokens from the from address to the dest address.
||| @from the address the tokens to be transferred from
||| @dest the address the tokens to be transferred to
||| @tokens the amount of tokens to be transferred
total performTransfer : (from : Address) -> (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error Storage
performTransfer from dest tokens storage =
  let fromBalance = getAccountBalance from (accounts storage)
      destBalance = getAccountBalance dest (accounts storage) in
        case lte tokens fromBalance of
             False => Left NotEnoughBalance
             True =>
               let accountsStored = insert from (MkAccount (minus fromBalance tokens) (getAccountAllowance from (accounts storage))) (accounts storage) in
                 Right
                   (record
                     {accounts =
                       insert dest (MkAccount (destBalance + tokens) (getAccountAllowance dest (accounts storage))) accountsStored
                     } storage)

-- Note: Nested record field update syntax(which is safer) doesn't seem to work
total updateAllowance : (allower : Address) -> (allowee : Address) -> (tokens : Nat) -> Storage -> Storage
updateAllowance allower allowee tokens storage =
  case tokens of
    Z => record
           {accounts =
              insert -- update allower's allowance map
              allower -- k
              (MkAccount -- v
                (getAccountBalance allower (accounts storage)) -- balance of allower is unchanged
                (delete allowee (getAccountAllowance allower (accounts storage))) -- delete allowee from allower's allowance map
              )
              (accounts storage) -- update the accounts map
           } storage
    n => record
           {accounts =
              insert
              allower -- k
              (MkAccount -- v
                (getAccountBalance allower (accounts storage)) -- balance
                (insert allowee n (getAccountAllowance allower (accounts storage))) -- allowance map
              )
              (accounts storage) -- accounts map
           } storage
||| approve update the allowance map of the caller of the contract
||| @spender the address the caller approve the tokens to transfer to
||| @tokens the amount of tokens approved to be transferred
total approve : (spender : Address) -> (tokens : Nat) -> Storage -> Storage
approve spender tokens storage =
  let caller = owner storage in -- caller is the caller of the contract, it is set to be the owner for now.
    updateAllowance caller spender tokens storage


||| createAccount transfers tokens from the owner to an address
||| @dest the address of the account to be created
||| @tokens the amount of tokens in the new created account
total createAccount : (dest : Address) -> (tokens : Nat) -> (storage : Storage) -> Either Error Storage
createAccount dest tokens storage =
  let owner = owner storage in
      case owner == owner of --when caller can be detected, check caller == owner.
           False => Left FailedToAuthenticate
           True => performTransfer owner dest tokens storage
