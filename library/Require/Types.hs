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
      { riFullModuleName :: ModuleName,
        riModuleAlias :: Text,
        riImportedTypes :: Maybe [Text]
      }
  deriving (Show)
