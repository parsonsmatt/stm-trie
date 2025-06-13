{-# language PolyKinds #-}
{-# language AllowAmbiguousTypes #-}

-- | A @'StaticTrie' k v@ has a composite key @k@ that can be decomposed
-- into a fixed structure. For the most part, this means a tuple-key, but
-- you could also define this for a record field.
module StmContainers.StaticTrie where

import Control.Monad
import Prelude hiding (lookup)
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import Data.Kind
import Control.Concurrent.STM
import Data.Hashable
import qualified Focus

data StaticTrieKeys xs v where
    StaticTrieKeyNil :: TVar (Maybe v) -> StaticTrieKeys '[] v
    StaticTrieKeyCons :: Hashable x => Map x (StaticTrieKeys xs v) -> StaticTrieKeys (x ': xs) v

data HList xs where
    HNil :: HList '[]
    HCons :: NewStaticTrieKeys ks => k -> HList ks -> HList (k ': ks)

data StaticTrie k v where
    MkStaticTrie :: StaticTrieKeys (ToStaticTrieKeys k) v -> StaticTrie k v

class NewStaticTrieKeys xs where
    new :: STM (StaticTrieKeys xs v)

instance NewStaticTrieKeys '[] where
    new =
        StaticTrieKeyNil <$> newTVar Nothing

instance (Hashable x) => NewStaticTrieKeys (x ': xs) where
    new = do
        StaticTrieKeyCons <$> Map.new

lookup :: HList xs -> StaticTrieKeys xs v -> STM (Maybe v)
lookup HNil (StaticTrieKeyNil mv) = readTVar mv
lookup (HCons k rest) (StaticTrieKeyCons m) = do
    mmap <- Map.lookup k m
    fmap join $ traverse (lookup rest) mmap

insert
    :: forall xs v.
    HList xs
    -> v -> StaticTrieKeys xs v -> STM ()
insert HNil v (StaticTrieKeyNil var) =
    writeTVar var (Just v)
insert (HCons (k :: k) (rest :: HList ks)) v (StaticTrieKeyCons m) = do
    Map.focus (go rest) k m
  where
    go (_ ) = do
        Focus.alterM $ \maybeInner ->
            Just <$> case maybeInner of
                Nothing ->
                    do
                        inner <- new @ks
                        insert rest v inner
                        pure inner
                Just inner -> do
                    insert rest v inner
                    pure inner

class StaticTrieKey k where
    type ToStaticTrieKeys k :: [Type]

instance StaticTrieKey Char where
    type ToStaticTrieKeys Char = '[Char]

