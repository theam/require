# [require][]

Most of the time we find ourselves following this pattern in Haskell:

```haskell
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
```

**require** adds a new keyword, `require` that abstracts this.
The former code snippet can be reduced to

```haskell
require Data.Text
require Data.ByteString
```

# How to use require

* Add `require` to your project dependencies
* Set the required GHC option:
  - `ghc-options: -F -pgmF requirepp` in your project configuration file.
  - `{-# OPTIONS_GHC -F -pgmF requirepp #-}` if you want it per-file.

