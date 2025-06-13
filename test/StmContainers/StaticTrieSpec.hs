{-# language BlockArguments #-}

module StmContainers.StaticTrieSpec where

import qualified StmContainers.StaticTrie as StaticTrie
import Control.Concurrent.STM
import Test.Hspec

spec :: Spec
spec = do
    describe "TrieKey Char" do
        describe "Can construct a Trie" do
            it "works" do
                trie <- atomically $ StaticTrie.new @'[Char] @Int
                atomically (StaticTrie.lookup (StaticTrie.HCons 'a' StaticTrie.HNil) trie)
                    `shouldReturn` Nothing
                atomically (StaticTrie.insert (StaticTrie.HCons 'a' StaticTrie.HNil) 1 trie)
                atomically (StaticTrie.lookup (StaticTrie.HCons 'a' StaticTrie.HNil) trie)
                    `shouldReturn` Just 1

