module FakeLib
-- this lib provides some fake standard smart contract functions.
%access public export

||| Address is the key hash of the owner of the associated account.
Address : Type
Address = String

||| currentCaller is the address of the caller of the current operation.
currentCaller : Address
currentCaller = "qwer"
