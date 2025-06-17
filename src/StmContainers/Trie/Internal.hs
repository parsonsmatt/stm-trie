{-# language StrictData #-}
{-# language QuantifiedConstraints #-}
{-# language BangPatterns #-}

module StmContainers.Trie.Internal where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.STM
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map
import Data.Hashable
import ListT (ListT)
import qualified ListT as ListT

-- | The internal representation of a 'Trie'.
--
-- @since 0.0.1.0
data TrieNode k v
    = Node (TVar (Maybe v)) (Map k (TrieNode k v))

-- | A @'Trie' k v@ is indexed by a @[k]@.
--
-- This is based on the underlying "StmContainers.Map" type, and so should
-- be used where concurrent access to a nested state is desirable.
--
-- @since 0.0.1.0
newtype Trie k v = Trie { unTrie :: TVar (TrieNode k v) }

-- | Create a new and empty 'Trie'.
--
-- @since 0.0.1.0
new :: STM (Trie k v)
new = do
    leaf <- Node <$> newTVar Nothing <*> Map.new
    Trie <$> newTVar leaf

-- | Create a new and empty 'Trie' in 'IO'. This is primarily useful for
-- creating unsafe global references, as 'atomically' cannot be called in
-- 'unsafePerformIO'.
--
-- @since 0.0.1.0
newIO :: IO (Trie k v)
newIO = do
    leaf <- Node <$> newTVarIO Nothing <*> Map.newIO
    Trie <$> newTVarIO leaf

-- | Insert the value at the path described.
--
-- The value is evaluated to WHNF before inserting.
--
-- @since 0.0.1.0
insert :: Hashable k => [k] -> v -> Trie k v -> STM ()
insert keys' !v t = do
    node <- readTVar (unTrie t)
    go keys' node
  where
    go [] (Node here _) =
        writeTVar here (Just v)
    go (k:ks) (Node _ there) = do
        -- I tried to use 'Map.focus' here, thinking that it would be
        -- faster. However, in my extremely dumb performance testing, this
        -- implementation was significantly faster.
        mn <- Map.lookup k there
        case mn of
            Nothing -> do
                there' <- Map.new
                here <- newTVar Nothing
                let newNode = Node here there'
                Map.insert newNode k there
                go ks newNode
            Just n' -> do
                go ks n'

lookup :: Hashable k => [k] -> Trie k v -> STM (Maybe v)
lookup keys' t = do
    node <- readTVar (unTrie t)
    go keys' node
  where
    go [] (Node here _) = do
        readTVar here
    go (k:ks) (Node _ rest) = do
        mn <- Map.lookup k rest
        case mn of
            Nothing ->
                pure Nothing
            Just n ->
                go ks n

listT
    :: (forall a. Monoid (f a), Applicative f)
    => Trie k v
    -> ListT STM (f k, v)
listT =
    listTGeneric readTVar Map.listT

toList
    :: (forall a. Monoid (f a), Applicative f)
    => Trie k v
    -> STM [(f k, v)]
toList = ListT.toList . listT

listTNonAtomic
    :: (forall a. Monoid (f a), Applicative f)
    => Trie k v
    -> ListT IO (f k, v)
listTNonAtomic =
    listTGeneric (atomically . readTVar) Map.listTNonAtomic

toListNonAtomic
    :: (forall a. Monoid (f a), Applicative f)
    => Trie k v
    -> IO [(f k, v)]
toListNonAtomic = ListT.toList . listTNonAtomic

listTGeneric
    :: (forall a. Monoid (f a), Applicative f, Monad m)
    => (forall x. TVar x -> m x)
    -> (forall v'. Map k v' -> ListT m (k, v'))
    -> Trie k v
    -> ListT m (f k, v)
listTGeneric readVar mapToListT = go mempty <=< lift . readVar . unTrie
  where
    go prefix (Node here there) = do
        ma <- lift $ readVar here
        maybe id (ListT.cons . (,) prefix) ma $ do
            (k, v) <- mapToListT there
            let prefix' = prefix <> pure k
            go prefix' v
