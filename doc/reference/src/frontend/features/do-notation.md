Actions in any monad can be sequenced with `do`-notation:

```
func : m Int
func = do
  one <- funcOne
  two <- funcTwo one
  return (two + 3)
```

`do`-notation desugars into `bind` (`>>=`) and `return` as defined by the monad instance.
