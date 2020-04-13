{-# LANGUAGE DeriveTraversable #-}
module Require.Types where

import Relude

-- | A type-safe wrapper around a fully qualified module name.
newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving (Eq, Show)


-- | Describes the different directives which require special processing during
-- the transformation step.
data RequireDirective
  = ModuleDirective ModuleName
  | RequireDirective RequireInfo
  | AutorequireDirective


-- | Describes the contents of a parsed @require@ directive.
--
-- @
-- require /riFullModuleName/ as /riModuleAlias/ (/riImportedTypes/)
-- @
data RequireInfo
  = RequireInfo
      { riFullModuleName :: !ModuleName,
        riModuleAlias :: !Text,
        riImportedTypes :: !Text
      }
  deriving (Show)


data AutorequireMode a
  = AutorequireEnabled a
    -- ^ Include the contents of the Requires file directly after the @module …
    -- where@ directive.

  | AutorequireOnDirective (Maybe a)
    -- ^ Include the contents of the Requires file when the user specifies the
    -- @autorequire@ directive.

  | AutorequireDisabled
    -- ^ Don't do any auto-requiring.
  deriving (Functor, Foldable, Traversable)
