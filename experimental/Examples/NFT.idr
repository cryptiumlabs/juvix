-- Example non-fungible token contract (ERC721) in Idris.
import Data.SortedMap
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
  tokenOwner : Address
  approved : Maybe Address -- approved address (there can at most be one address)

||| The storage has type Storage which is a record with accounts and tokens.
record Storage where
    constructor MkStorage
    accounts : SortedMap Address Account
    tokens : SortedMap TokenId Token
    version : Nat -- version of the token standard
    totalSup : Nat
    owner : Address -- owner of this NFT contract

total emptyStorage : Storage
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
              let newAcc = insert dest (record {ownedTokens $= (+ 1)} destAcc) acc
                  newToken = insert token (MkToken dest Nothing) (tokens storage) in
                    Right
                      (record
                        {accounts = newAcc,
                         tokens = newToken,
                         totalSup $= (+ 1)
                        } storage
                      )

total initStorage : Storage
initStorage =
  case mint 1 "qwer" emptyStorage of
    Left _ => emptyStorage
    Right s => s

total storage : Storage
storage =
  initStorage

total balanceOf : Address -> Nat
balanceOf address =
  getAccountBal address (accounts storage)

ownerOf : TokenId -> Either Error Address
ownerOf token =
  case lookup token (tokens storage) of
    Nothing => Left NonExistenceToken
    Just t => Right (tokenOwner t)

||| rightOwnerOf is intended for abstracting Right of ownerOf.
||| Only use this when ownerOf cannot return Left.
rightOwnerOf : Either Error Address -> Address
rightOwnerOf (Left _) = owner storage
rightOwnerOf (Right owner) = owner

||| ownerOfToken uses rightOwnerOf to abstract Right of ownerOf.
||| Only use this when ownerOf cannot return Left.
total ownerOfToken : TokenId -> Address
ownerOfToken =
  rightOwnerOf . ownerOf

total getApproved : TokenId -> Either Error (Maybe Address)
getApproved token =
  case lookup token (tokens storage) of
    Nothing => Left NonExistenceToken
    Just t => Right (approved t)

||| approve set the approved address for an NFT.
||| When a transfer executes, the approved
||| address for that NFT (if any) is reset to none.
total approve : Address -> TokenId -> Either Error Storage
approve address token =
  case getApproved token of
    Left e => Left e
    Right approvedAdd =>
      let isApproved = currentCaller == (maybe (owner storage) id approvedAdd)
          owner = ownerOfToken token in
          case currentCaller == owner || isApproved of
            False => Left FailedToAuthenticate
            True =>
              Right
                (record
                   {tokens =
                     insert
                     token
                     (MkToken
                       owner
                       (Just address)
                     )
                     (tokens storage)
                   } storage
                 )
--figuring out nested record update, doesn't work atm
-- newAcc : Address -> Token
-- newAcc add =
--   record
--     {approved -> tokens =
--         Jusy add
--     } (tokens storage)

total newOp : Address -> Bool -> SortedMap Address Bool
newOp operator isSet =
  case lookup currentCaller (accounts storage) of
    Nothing => insert operator isSet empty
    Just op => insert operator isSet (opApprovals op)

||| setApprovalForAll let the owner enable or disable an operator.
||| The operator can manage all NFTs of the owner.
total setApprovalForAll : (operator : Address) -> Bool -> Storage
setApprovalForAll operator isSet =
    record
      {accounts =
        insert
        currentCaller
        (MkAccount
          (balanceOf currentCaller)
          (newOp operator isSet)
        )
        (accounts storage)
      } storage

-- total isApprovedForAll : (owner : Address) -> (operator : Address) -> Bool
-- isApprovedForAll owner operator =

--
-- ||| transfer transfers a NFT from the from address to the dest address.
-- ||| @from the address the tokens to be transferred from
-- ||| @dest the address the tokens to be transferred to
-- ||| @token the TokenId of the NFT to be transferred
-- total transfer :
-- (from : Address) -> (dest : Address) -> (token : TokenId) -> Either Error Storage
-- transfer from dest token =
--   case ownerOf token of
--     Left e => Left e
--     Right add =>
--       case add == from of
--         False => Left NotOwnedByFromAddress
--         True =>
--           case currentCaller == from ||
--                currentCaller == getApproved token ||
--                currentCaller == isApprovedForAll of
--             False => Left FailedToAuthenticate
--             True =>
--               --set dest ownedTokens to $= (+1)
--               --set tokenOwner address to dest
--               --set approved address to none
