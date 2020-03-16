module FakeLib
-- this lib provides some fake standard smart contract data types and functions.
%access public export --has to do public export otherwise missing Address instances

||| Address is the key hash of the owner of the associated account.
Address : Type
Address = String

||| Error errors
data Error = NotEnoughBalance
           | FailedToAuthenticate
           | NotAllowedToSpendFrom
           | NotEnoughAllowance
           | TokenAlreadyMinted
           | NonExistenceToken
           | NotOwnedByFromAddress

||| currentCaller is the address of the caller of the current operation.
currentCaller : Address
currentCaller = "qwer"
