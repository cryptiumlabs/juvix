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
total getAccount :
(address : Address) -> SortedMap Address Account -> Account
getAccount address accounts = case lookup address accounts of
                      Nothing => MkAccount 0 empty
                      (Just account) => account

||| getAccountBal returns the balance of an associated key hash.
||| @address the key hash of the owner of the account
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
  let currentAcc = accounts storage
      currentTok = tokens storage in
    let destAcc = getAccount dest currentAcc in
      case lookup token currentTok of
        Just something => Left TokenAlreadyMinted
        Nothing =>
          case currentCaller == owner storage of
            False => Left FailedToAuthenticate
            True =>
              let newAcc = insert dest (record {ownedTokens $= (+ 1)} destAcc) currentAcc
                  newToken = insert token (MkToken dest Nothing) currentTok in
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

total currentAcc : SortedMap Address Account
currentAcc = accounts storage

total currentTok : SortedMap TokenId Token
currentTok = tokens storage

||| balanceOf [standard function]
||| returns the number of NFTs owned by the input address.
total balanceOf : Address -> Nat
balanceOf address =
  getAccountBal address currentAcc

||| ownerOf [standard function]
||| returns the address of the owner of the input NFT.
ownerOf : TokenId -> Either Error Address
ownerOf token =
  case lookup token currentTok of
    Nothing => Left NonExistenceToken
    Just t => Right (tokenOwner t)

||| ownerOfToken abstracts Right of ownerOf.
||| Only use this when ownerOf cannot return Left.
total ownerOfToken : TokenId -> Address
ownerOfToken token =
  case ownerOf token of
    Left e => owner storage
    Right a => a

||| getApproved [standard function]
||| get the approved address for the input NFT
total getApproved : TokenId -> Either Error (Maybe Address)
getApproved token =
  case lookup token currentTok of
    Nothing => Left NonExistenceToken
    Just t => Right (approved t)

||| approvedAdd abstracts Right of getApproved.
||| Only use this when getApproved cannot return Left.
total approvedAdd : TokenId -> Maybe Address
approvedAdd token =
  case getApproved token of
    Left e => Just (owner storage)
    Right a => a

||| approve [standard function] set the approved address for an NFT.
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
                     currentTok
                   } storage
                 )

||| newOp function to update the operator list.
total newOp : Address -> Bool -> SortedMap Address Bool
newOp operator isSet =
  case lookup currentCaller currentAcc of
    Nothing => insert operator isSet empty
    Just op => insert operator isSet (opApprovals op)

||| setApprovalForAll [standard function]
||| let the owner enable or disable an operator.
||| The operator can manage all NFTs of the owner.
total setApprovalForAll : (operator : Address) -> Bool -> Either Error Storage
setApprovalForAll operator isSet =
  case currentCaller == operator of
    True => Left OwnerCannotBeOperator
    False =>
      Right
        (record
          {accounts =
            insert
            currentCaller
            (MkAccount
              (balanceOf currentCaller)
              (newOp operator isSet)
            )
            currentAcc
          } storage
         )

||| isApprovedForAll [standard function]
||| returns whether an address is an authorized operator for another address.
||| @owner the address that owns the NFTs.
||| @operator the address that acts on behalf of the owner.
total isApprovedForAll : (owner : Address) -> (operator : Address) -> Bool
isApprovedForAll owner operator =
  case lookup owner currentAcc of
    Nothing => False
    Just op =>
      case lookup operator (opApprovals op) of
        Nothing => False
        Just bool => bool

||| modifyAccBal function to modify the account balance.
total modifyAccBal :
Address -> (Nat -> Nat) -> SortedMap Address Account -> SortedMap Address Account
modifyAccBal add f acc =
  let addAcc = getAccount add acc in
    insert add (record {ownedTokens $= f} addAcc) acc

||| transfer [standard function]
||| transfers the ownership of a NFT from one address to another.
||| @from the address the tokens to be transferred from
||| @dest the address the tokens to be transferred to
||| @token the TokenId of the NFT to be transferred
total transfer :
(from : Address) -> (dest : Address) -> (token : TokenId) -> Either Error Storage
transfer from dest token =
  case ownerOf token of
    Left e => Left e
    Right add =>
      case add == from of
        False => Left NotOwnedByFromAddress
        True =>
          case currentCaller == from ||
               currentCaller == maybe (owner storage) id (approvedAdd token) ||
               isApprovedForAll add currentCaller of
            False => Left FailedToAuthenticate
            True =>
              let newAcc = modifyAccBal from (minus 1) (modifyAccBal dest (+ 1) currentAcc)
                  newToken = insert token (MkToken dest Nothing) currentTok in
                Right
                  (record
                    {accounts = newAcc,
                    tokens = newToken} storage
                  )
