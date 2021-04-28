# PLONK IR

This is mainly a translation of https://hackmd.io/@zacwilliamson/r1z1yZTs8 into Haskell

## Binary operations

| Opcode | Opcode Bytes    | Types                   | Description     | Notes |
| ------ | --------------- | ----------------------- | --------------- | ----- |
| add    | `[a], [b], [o]` | field -> field -> field | o = a + b       |       |
| sub    | `[a], [b], [o]` | field -> field -> field | o = a - b       |       |
| mul    | `[a], [b], [o]` | field -> field -> field | o = a * b       |       |
| div    | `[a], [b], [o]` | field -> field -> field | o = a / b       |       |
| mod    | `[a], [b], [o]` | field -> field -> field | o = a % b       |       |
| and    | `[a], [b], [o]` | bool -> bool -> bool    | `o = a &amp; b` |       |
| or     | `[a], [b], [o]` | bool -> bool -> bool    | `o = a | b`     |       |
| xor    | `[a], [b], [o]` | bool -> bool -> bool    | `o = a ^ b`     |       |

```haskell
data BinOp f a where
  BAdd :: BinOp f f
  BSub :: BinOp f f
  BMul :: BinOp f f
  BDiv :: BinOp f f
  BMod :: BinOp f f
  BExp :: BinOp f f
  BAnd :: BinOp f Bool
  BOr :: BinOp f Bool
  BXor :: BinOp f Bool
```

## Comparing operations

These are also binary operations with the constraint that the resulting value is of type `Boolean`.

| Opcode | Opcode Bytes    | Description       | Notes |
| ------ | --------------- | ----------------- | ----- |
| gt     | `[a], [b], [o]` | `o = (a &gt; b)`  |       |
| gte    | `[a], [b], [o]` | `o = (a &gt;= b)` |       |
| lt     | `[a], [b], [o]` | `o = (a &lt; b)`  |       |
| lte    | `[a], [b], [o]` | `o = (a &lt;= b)` |       |
| eq     | `[a], [b], [o]` | `o = (a == b)`    |       |

```haskell
data CompOp f where
  CGt :: CompOp f
  CGte :: CompOp f
  CLt :: CompOp f
  CLte :: CompOp f
  CEq :: CompOp f
```

## Unary operations

| Opcode   | Opcode Bytes  | Description                    | Notes                 |
| -------- | ------------- | ------------------------------ | --------------------- |
| setpub   | `[p], [o]`    | o = p                          | from public calldata  |
| setpriv  | `[p], [o]`    | o = p                          | from private calldata |
| iszero   | `[a], [o]`    | `o = (a == 0)`                 |                       |
| not      | `[a], [o]`    | `o = !a`                       |                       |
| shl      | `[a], x, [o]` | `o = a &lt;&lt; x`             | x is const            |
| shr      | `[a], x, [o]` | `o = a &gt;&gt; x`             |                       |
| rotl     | `[a], x, [o]` | `o = a &lt;&lt;&lt; x`         |                       |
| rotr     | `[a], x, [o]` | `o = a &gt;&gt;&gt; x`         |                       |
| asserteq | `[a], [o]`    | throw if `a != 0`              |                       |
| assertlt | `[a], x`      | throw if `a &gt;= 2^x`         |                       |

```haskell
data UnOp f a where
  UIsZero :: UnOp f Bool
  UNot :: UnOp f Bool
  UShL :: Int -> UnOp f f
  UShR :: Int -> UnOp f f
  URotL :: Int -> UnOp f f
  URotR :: Int -> UnOp f f
  UAssertEq :: UnOp f Bool
  UAssertIt :: UnOp f Bool
```


## Other operations

| Opcode     | Opcode Bytes                         | Description                                  | Notes                         |
| ---------- | ------------------------------------ | -------------------------------------------- | ----------------------------- |
| accumulate | `x, [...a], ...q, [o]`               | $o = \sum_i^xa_iq_i$                         |                               |
| gadd       | `[x1], [y1], [x2], [y2], [x3], [y3]` | `(x3, y3) = (x1, y1)` $\bigoplus$ `(x2, y2)` | elliptic curve point addition |

## IR AST (Haskell)
``` haskell
-- | Intermediate representation of (arithmetic) expressions over a field @f@
-- with variable names/indices coming from @i@. @ty@ is the resulting value.
data IR i f ty where
  IConst :: f -> IR i f f
  IVar :: i -> IR i f f
  IUnOp :: UnOp f ty -> IR i f ty -> IR i f ty
  IBinOp :: BinOp f ty -> IR i f ty -> IR i f ty -> IR i f ty
  ICompOp :: CompOp f -> IR i f f -> IR i f f -> IR i f Bool
  IAcc :: [IR i f ty] -> [ty] -> IR i f ty
  IECAdd :: IR i f (f, f) -> IR i f (f, f) -> IR i f (f, f)
```