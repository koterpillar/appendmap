# appendmap

A `Data.Map` wrapper with a `Semigroup` and `Monoid` instances that delegate to
the individual keys.

```haskell
import qualified Data.Map as Map
import Data.Map.Append

> one = AppendMap $ Map.fromList [(1, "hello"), (2, "goodbye")]
> two = AppendMap $ Map.fromList [(1, "world"), (3, "again")]

> unAppendMap (one <> two)
fromList [(1, "helloworld"), (2, "goodbye"), (3, "again")]
```

## Motivation

`Data.Map` has a `Semigroup` instance that keeps the values from the first
argument in case of a key conflict:

```haskell
import qualified Data.Map as Map
import Data.Map.Append

> Map.fromList [(1, "hello")] <> Map.fromList [(1, "goodbye")]
fromList [(1, "hello")]
```

A different instance has been suggested for a long time
([1](https://mail.haskell.org/pipermail/libraries/2012-April/017743.html),
[2](https://ghc.haskell.org/trac/ghc/ticket/1460)), but this is a breaking change and hasn't happened yet (as of 2018-08).
