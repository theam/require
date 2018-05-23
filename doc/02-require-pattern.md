---
title: The require pattern
---

# The require pattern

With **require**, you are advised to design your modules as if they were to be imported
qualified. As [Jasper Van der Jeugt](https://jaspervdj.be/) suggests [in his talk](https://jaspervdj.be/posts/2017-12-07-getting-things-done-in-haskell.html).

# Example

Instead of writing a module like this

```haskell top
data Person = Person
  { personName :: String
  , personAge  :: Int
  }

getPersonCode :: Person -> String
getPersonCode p =
  personName p <> (show $ personAge p)
```

_(`String` used for simplification)_

You are advised to put all these definitions in a separate module. Calling it `Person`,
and removing all the prefixes:

```haskell
module Person where

data Person = Person
  { name :: String
  , age  :: Int
  }

getCode :: Person -> String
getCode p =
  name p <> (show $ age p)
```

This way, the code becomes simpler, and allows you to separate concerns in larger applications.

## Using the person module from another one

Now, to use these definitions, we use **require**.

```haskell
require Person

foo :: Person -> Person -> Int
foo p1 p2 =
  length (Person.getCode p1 <> Person.name p2)
```


