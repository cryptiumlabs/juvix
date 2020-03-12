-- Example non-fungible token contract (ERC721) in Idris.
import Data.SortedMap
import Data.Vect
import FakeLib

||| TokenId is the unique identifier for each NFT
TokenId : Type
TokenId = Nat

||| Account contains the balance (number of tokens owned)
||| and a map of operator approvals of an associated address.
record Account where
  constructor MkAccount
  ownedTokens : Nat
  opApprovals : SortedMap Address Bool --operator approvals

||| Token contains the info of a specific token
record Token where
  constructor MkToken
  owner : Address
  approved : Vect n Address -- approved addresses

||| The storage has type Storage which is a record with accounts and tokens.
record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    tokens : SortedMap TokenId Token
    version : Nat -- version of the token standard
    totalSup : Nat
    owner : Address -- owner of this NFT contract

data Error = FailedToAuthenticate
           | TokenAlreadyMinted
           | NotAllowedToSpendFrom

emptyStorage : Storage
emptyStorage =
  MkStorage
    empty
    empty
    1
    0
    "qwer"

||| getAccount returns the account of an associated key hash.
||| @address the key hash of the owner of the account
total getAccount : (address : Address) -> SortedMap Address Account -> Account
getAccount address accounts = case lookup address accounts of
                      Nothing => MkAccount 0 empty
                      (Just account) => account

total getAccountBal : (address : Address) -> SortedMap Address Account -> Nat
getAccountBal address accounts = case lookup address accounts of
                      Nothing => 0
                      (Just account) => ownedTokens account

||| mint when the caller is the owner of the token contract,
||| they can mint/add new tokens to the total supply
||| @token the token to be added to the total supply
||| @dest the address the new tokens to be added to
||| @storage the original storage
total mint :
(token : TokenId) -> (dest : Address) -> (storage : Storage) -> Either Error Storage
mint token dest storage =
  let acc = accounts storage in
    let destAcc = getAccount dest acc in
      case lookup token (tokens storage) of
        Just something => Left TokenAlreadyMinted
        Nothing =>
          case currentCaller == owner storage of
            False => Left FailedToAuthenticate
            True =>
              let newAcc = insert dest (record {ownedTokens = (1 + getAccountBal dest acc)} destAcc) acc
                  newToken = insert token (MkToken dest Nil) (tokens storage) in
                    Right
                      (record
                        {accounts = newAcc,
                         tokens = newToken,
                         totalSup = (totalSup storage) + 1
                        } storage
                      )

initStorage : Storage
initStorage =
  case mint 1 "qwer" emptyStorage of
    Left _ => emptyStorage
    Right s => s

{-

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
