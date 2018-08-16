# appendmap

A `Data.Map` wrapper with a `Monoid` instance that delegates to the individual keys.

```haskell
import qualified Data.Map as Map

> one = AppendMap $ Map.fromList [(1, "hello"), (2, "goodbye")]
> two = AppendMap $ Map.fromList [(1, "world"), (3, "again")]

> unAppendMap (one <> two)
fromList [(1, "helloworld"), (2, "goodbye"), (3, "again")]
```
