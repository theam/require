---
title: Automatic require
---

# Autorequire

Autorequire helps you importing the things you want in every file of your project. 

We will need a **Requires** file in our project root directory and then, use `autorequirepp` instead of `requirepp`:

- `ghc-options: -F -pgmF autorequirepp` in your project configuration file.
- `{-# OPTIONS_GHC -F -pgmF autorequirepp #-}` if you want it per-file.

This way, require will automatically read the **Requires** file and paste all those imports in every file in the project.

