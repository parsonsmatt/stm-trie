{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData #-}

-- | This module contains internals to the package. Breaking changes here
-- will not be reflected as part of the major version of this package.
--
-- @since 0.0.1.0
module StmContainers.Trie.Internal where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Data.Hashable
import Data.Maybe
import qualified Focus
import ListT (ListT)
import qualified ListT as ListT
import StmContainers.Map (Map)
import qualified StmContainers.Map as Map

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
newtype Trie k v = Trie {unTrie :: TVar (TrieNode k v)}

overTrie :: (TrieNode k v -> STM r) -> Trie k v -> STM r
overTrie k (Trie t) =
    readTVar t >>= k

-- | Create a new and empty 'Trie'.
--
-- @since 0.0.1.0
new :: STM (Trie k v)
new = do
    leaf <- mkNewNode Nothing
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
insert :: (Hashable k) => [k] -> v -> Trie k v -> STM ()
insert keys !v = overTrie (go keys)
  where
    go [] (Node here _) =
        writeTVar here (Just v)
    go (k : ks) (Node _ there) = do
        -- I tried to use 'Map.focus' here, thinking that it would be
        -- faster. However, in my extremely dumb performance testing, this
        -- implementation was significantly faster.
        mn <- Map.lookup k there
        case mn of
            Nothing -> do
                newNode <- mkNewNode Nothing
                Map.insert newNode k there
                go ks newNode
            Just n' -> do
                go ks n'

-- | Lookup an element at the path given by @[k]@.
--
-- @since 0.0.1.0
lookup :: (Hashable k) => [k] -> Trie k v -> STM (Maybe v)
lookup = overTrie . go
  where
    go [] (Node here _) = do
        readTVar here
    go (k : ks) (Node _ rest) = do
        mn <- Map.lookup k rest
        case mn of
            Nothing ->
                pure Nothing
            Just n ->
                go ks n

-- | Deletes the entry located precisely at the path determined by @[k]@.
-- This will not delete other entries that have @[k]@ as a prefix. For
-- that, see 'deleteUnder'.
--
-- @since 0.0.1.0
delete :: (Hashable k) => [k] -> Trie k v -> STM ()
delete = overTrie . go
  where
    go [] (Node here _) =
        writeTVar here Nothing
    go (k : ks) (Node _ there) = do
        mn <- Map.lookup k there
        forM_ mn (go ks)

-- | Delete the value located at the path @[k]@ and also all children of
-- that path.
--
-- To delete an entire 'Trie', you can write @'deleteChildren' []@.
--
-- @since 0.0.1.0
deleteChildren :: (Hashable k) => [k] -> Trie k v -> STM ()
deleteChildren = overTrie . go
  where
    go [] (Node here there) = do
        writeTVar here Nothing
        Map.reset there
    go (k : ks) (Node _ there) = do
        traverse_ (go ks) =<< Map.lookup k there

-- | Delete the entire 'Trie'.
--
-- @since 0.0.1.0
reset :: (Hashable k) => Trie k v -> STM ()
reset = deleteChildren []

-- | Count the elements in the 'Trie'.
--
-- This is $$O(n)$$ in the size of the 'Trie'.
--
-- @since 0.0.1.0
size :: Trie k v -> STM Int
size = overTrie (go 0)
  where
    go !acc (Node here there) = do
        ma <- readTVar here
        let
            here' = if isJust ma then acc + 1 else acc
        ListT.fold (\ !acc' (_, n) -> go acc' n) here' $ Map.listT there
{-# INLINEABLE size #-}

-- | Return whether or not the 'Trie' has an elements.
--
-- @since 0.0.1.0
null :: Trie k v -> STM Bool
null = overTrie go
  where
    go (Node here there) = do
        ma <- readTVar here
        if isJust ma
            then pure False
            else do
                thereEmpty <- Map.null there
                if thereEmpty
                    then pure True
                    else do
                        ListT.foldMaybe f False $ Map.listT there

    f seen (_, v) = do
        if seen
            then pure Nothing
            else do
                kNull <- go v
                if kNull
                    then pure (Just kNull)
                    else pure Nothing

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
            let
                !prefix' = prefix <> pure k
            go prefix' v

-- | Apply a 'Focus' to the path of the keys @[k]@.
--
-- @since 0.0.1.0
focus :: (Hashable k) => Focus.Focus v STM r -> [k] -> Trie k v -> STM r
focus (Focus.Focus conceal reveal) = overTrie . go
  where
    go [] (Node here _) = do
        mv <- readTVar here
        case mv of
            Nothing -> do
                (result, change) <- conceal
                case change of
                    Focus.Leave ->
                        pure ()
                    Focus.Remove ->
                        pure ()
                    Focus.Set !a ->
                        writeTVar here (Just a)
                pure result
            Just v -> do
                (result, change) <- reveal v
                case change of
                    Focus.Leave ->
                        pure ()
                    Focus.Remove ->
                        writeTVar here Nothing
                    Focus.Set !a ->
                        writeTVar here (Just a)
                pure result
    go (k : ks) (Node _ there) = do
        mn <- Map.lookup k there
        case mn of
            Nothing -> do
                (result, change) <- conceal
                case change of
                    Focus.Leave ->
                        pure ()
                    Focus.Remove ->
                        pure ()
                    Focus.Set !a -> do
                        newNode <- mkNewNode (Just a)
                        Map.insert newNode k there
                pure result
            Just n ->
                go ks n

-- | Use a 'Focus' on the path @[k]@ provided, with access to the children.
--
-- @since 0.0.1.0
focusChildren
    :: (Hashable k)
    => Focus.Focus (Maybe v, Map k (TrieNode k v)) STM r -> [k] -> Trie k v -> STM r
focusChildren (Focus.Focus _ reveal) [] t = do
    let
        trieVar = unTrie t
    Node here there <- readTVar trieVar
    mv <- readTVar here
    (result, change) <- reveal (mv, there)
    case change of
        Focus.Leave ->
            pure ()
        Focus.Remove -> do
            writeTVar here Nothing
            Map.reset there
        Focus.Set (mv', newThere) -> do
            newNode <- Node <$> newTVar mv' <*> pure newThere
            writeTVar trieVar newNode
    pure result
focusChildren (Focus.Focus conceal reveal) (k : ks) t = do
    let
        trieVar = unTrie t
    Node _ there <- readTVar trieVar
    mnode <- Map.lookup k there
    case mnode of
        Nothing -> do
            (result, change) <- conceal
            case change of
                Focus.Leave ->
                    pure ()
                Focus.Remove ->
                    pure ()
                Focus.Set (mv', newThere) -> do
                    newNode <- Node <$> newTVar mv' <*> pure newThere
                    Map.insert newNode k there
            pure result
        Just n ->
            go there k ks n
  where
    go prevMap prevKey [] (Node here there) = do
        mv <- readTVar here
        (result, change) <- reveal (mv, there)
        case change of
            Focus.Leave ->
                pure ()
            Focus.Remove -> do
                writeTVar here Nothing
                Map.reset there
            Focus.Set (mv', newThere) -> do
                newNode <- Node <$> newTVar mv' <*> pure newThere
                Map.insert newNode prevKey prevMap
        pure result
    go _ _ (k' : ks') (Node _ there) = do
        mnode <- Map.lookup k' there
        case mnode of
            Nothing -> do
                (result, change) <- conceal
                case change of
                    Focus.Leave ->
                        pure ()
                    Focus.Remove -> do
                        pure ()
                    Focus.Set (mv', newThere) -> do
                        newNode <- Node <$> newTVar mv' <*> pure newThere
                        Map.insert newNode k' there
                pure result
            Just node ->
                go there k' ks' node

-- | Internal. Create a new 'TrieNode' and evaluate the contents of the
-- inner 'Maybe' to WHNF.
--
-- @since 0.0.1.0
mkNewNode :: Maybe v -> STM (TrieNode k v)
mkNewNode Nothing = do
    Node
        <$> newTVar Nothing
        <*> Map.new
mkNewNode (Just !a) = do
    Node
        <$> newTVar (Just a)
        <*> Map.new
