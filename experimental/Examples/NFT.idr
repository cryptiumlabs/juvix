-- Example non-fungible token contract (ERC721) in Idris.
import Data.SortedMap
import Data.Vect
import FakeLib

||| TokenId is the unique identifier for each NFT
TokenId : Type
TokenId = Nat

||| Account is a dependent record on the balance (the number of NFT owned)
||| by an associated address. It contains
||| a vector of of tokenIds owned and a map of operators approved.
record Account where
  constructor MkAccount
  ownedTokens : Vect bal TokenId --vect of tokenIds owned
  opApprovals : SortedMap Address Bool --operator approvals

||| Token contains the
record Token where
  constructor MkToken
  owner : Address
  approved : Vect n Address --approved addresses

||| The storage has type Storage which is a record with accounts and tokens.
record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    tokens : SortedMap TokenId Token

data Error = FailedToAuthenticate
           | NotAllowedToSpendFrom

initStorage : Storage
initStorage =
  MkStorage
    (insert "qwer" (MkAccount Nil empty) empty)
    (insert 1 (MkToken "qwer" Nil) empty)
{-
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
  let caller = owner storage in -- caller of the contract is set to be the owner for now.
    updateAllowance caller spender tokens storage

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
-}
