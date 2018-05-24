---
title: How require works
---

# How `require` works

**require** is a Haskell preprocessor.

Every source file will be threaded through **require** before compiling it with GHC.
This allows to add a `require` keyword that handles imports.

## Why not Template Haskell?

Because tracking down import generation is complicated, Template Haskell doesn't allow import
generation.

**require**s case for imports is very narrowed down and doesn't do crazy things like importing
many different modules with many different names.


