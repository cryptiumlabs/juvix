module Juvix.INetIR.Types where

import Juvix.Library

type Address = Int

type Slot = Int

data Port
  = -- Port, referencing another node at a particular address & slot.
    Port
      { portAddress ∷ Address,
        portSlot ∷ Slot
      }
  deriving (Show, Eq, Generic, Typeable)

type Kind dataTy = Int

data Node dataTy
  = Node { nodeAddress :: Address, nodeKind :: Kind dataTy }
  deriving (Show, Eq, Generic, Typeable)

{-

data Kind dataTy
  = -- Eraser node.
    Eraser
  | -- Constructor node, with integer label.
    Constructor
      { constructorLabel ∷ Integer
      }
  | -- Bespoke function node, possibly curried, with integer function reference.
    Bespoke
      { curriedFunction ∷ Integer,
        curriedData ∷ [dataTy]
      }
  | -- Data node, with data value.
    Data
      { dataValue ∷ dataTy
      }
  deriving (Show, Eq, Generic, Typeable)

data Node dataTy
  = -- Node, with address, kind, and ports.
    Node
      { nodeAddress ∷ Address,
        nodeKind ∷ Kind dataTy,
        nodePorts ∷ [Port]
      }
  deriving (Show, Eq, Generic, Typeable)

-}

data BespokeFunction opType
  = -- Bespoke function, with a unique number and an operation.
    BespokeFunction
      { functionNumber ∷ Integer,
        functionOp ∷ opType
      }
  deriving (Show, Eq, Generic, Typeable)

data Net dataTy opType
  = -- Interaction net, with many nodes and definitions for all bespoke functions.
    Net
      { netNodes ∷ [Node dataTy],
        netBespokeFunctions ∷ [BespokeFunction opType]
      }
  deriving (Show, Eq, Generic, Typeable)
