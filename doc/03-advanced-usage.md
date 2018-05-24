---
title: Advanced usage
---

# Aliasing

Sometimes we need to use a different alias for our imports, for example, if we were to do this:

```haskell
require Data.HashMap.Strict
```

**require** would desugar it into

```haskell
import Data.HashMap.Strict (Strict)
import qualified Data.HashMap.Strict as Strict
```

Which does not make much sense.

**require** allows you to make a custom alias for your modules:

```haskell
require Data.HashMap.Strict as HM
```

Will be desugared into:

```haskell
import Data.HashMap.Strict (Strict)
import qualified Data.HashMap.Strict as HM
```

Still, there is the problem of the type import:

# Type imports

**require** allows you to specify which types you want to import from the module you are requiring:

```haskell
require Data.HashMap.Strict (HashMap)
```

Will be desugared into:

```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Strict
```

So putting everything together, the full require for this module is:

```haskell
require Data.HashMap.Strict as HM (HashMap)
```
Will be desugared into:

```haskell
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
```

