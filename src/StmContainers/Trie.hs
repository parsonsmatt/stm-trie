-- | This module contains the interface for working with a @'Trie' k v@
-- a concurrent map of @[k]@ key paths to @v@ values.
--
-- This library is based on the "StmContainers.Map" module, and should
-- enjoy the performance benefits of that type.
--
-- @since 0.0.1.0
module StmContainers.Trie
    ( -- * Datatype
      Trie

      -- * Creation
    , new
    , newIO

      -- * Query
    , lookup
    , null
    , size

      -- * Modification
    , insert
    , delete
    , deleteChildren
    , reset
    , focus
    , focusChildren

      -- * Consuming
    , toList
    , toListNonAtomic
    , listT
    , listTNonAtomic
    , listTGeneric
    ) where

import StmContainers.Trie.Internal
import Prelude ()
