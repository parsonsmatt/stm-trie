# Initial Development

Date: Friday, June 13th, 2025

I need this to implement a more space and time efficient data structure for Prometheus metrics.
We're building absolutely enormous `Data.Map.Map` and storing them in an `IORef`, performing modifications with `atomicModifyIORef` to record metrics.
This puts a memory barrier on every HTTP request and significant contention on that `IORef`.

Replacing with an `STMContainers.Map (Text, Text, Text, Text)` or similar would *help* by removing the concurrent write problem, but it retains the space problem required to hold on to that many `Text` values.
So, instead, I want to make a datatype that uses the concurrent efficiency of `StmContainers.Map`, but also represents composite keys as nested maps.

For example, a `Trie Text Int` should effectively be a `Map Text Int`.
But a `Trie (Text, Text) Int` would be better as a `Map Text (Map Text Int)`.
However, consider a `Trie [Char] Int`.
This could be represented as a `Map Char (Either Int (Trie [Char] Int))` - for each `Char` in a dynamic length key, we may have either a terminal *or* a nest.
Actually, we may have *both* - consider,

```haskell
Trie.fromList
    [ ("a", 1)
    , ("aa", 2)
    , ("ab", 3)
    , ("ba", 4)
    ]
```

This should be represented, internally, as:

```haskell
Nest $
    Map.fromList
        [ ( 'a'
          , ( Just 1
            , Map.fromList
              [ ( 'a'
                , ( Just 2
                  , Map.empty
                  )
                )
              , ( 'b'
                , ( Just 3
                  , Map.empty
                  )
                )
              ]
            )
        , ( 'b'
          , ( Nothing
            , Map.fromList
                [ ( 'a'
                  , ( Just 4
                    , Map.empty
                    )
                  )
                ]
```

So there is a representational difference between keys of static decomposition (ie tuples) and keys of dynamic composition (ie lists).

I primarily require the *static* decomposition, where the structure and depth of the `Trie` is known at compile-time.
This is a simpler problem to solve, and so I will focus on it.
Additionally, static tries should have better performance, since we don't need to worry about possible early termination, and that means fewer pointer indirections.

# Static Trie

Let us consider the following concrete cases.

```haskell
type StaticTrie Int Char =
    Map Int Char

type StaticTrie (Int, Int) Char = 
    Map Int (Map Int Char)

type StaticTrie (Int, Int, Int) Char =
    Map Int (Map Int (Map Int Char))
```

We could define the logic for tuples directly.
However, this appears to be a case where recursion over an `HList` may be much more convenient.
We can create a class `ToHList` and this will help make this more reasonable.

The form I ended up with is:

```haskell
data StaticTrieNode xs v where
    StaticTrieKeyNil 
        :: TVar (Maybe v) 
        -> StaticTrieNode '[] v
    StaticTrieKeyBranch 
        :: Hashable x 
        => Map x (StaticTrieNode xs v) 
        -> StaticTrieNode (x ': xs) v

data HList xs where
    HNil :: HList '[]
    HCons :: NewStaticTrieNode ks => k -> HList ks -> HList (k ': ks)
```

I'm not too thrilled with it, but it did let me define `lookup` and `insert`.

# Actually...

OK, so there's static tries and dynamic tries.
But I think dynamic tries are actually probably much easier to implement and deal with, and they're more generally useful.

The big difference is that a dynamic trie must also contain a `TVar (Maybe a)` at each branch.
But, because there's no fixed type-level list of keys, we also don't need to conflate our types with the structure of the key fragments.

```haskell
data DynamicTrieNode k v
    = Leaf (TVar (Maybe a))
    | Branch (TVar (Maybe a)) (Map k (DynamicTrieNode k v))
```

This results in a simpler API `lookup :: Hashable k => [k] -> DynamicTrieNode k v -> STM (Maybe v)`.
`new` and `insert` can also use list keys here, which makes this all quite easy.
Interestingly enough, this also means that `lookup @String` will be an alphabet trie automatically.

Actually, `Leaf` and `Branch` are unnecessary - the possibility of emptiness may be represented as an empty map.
