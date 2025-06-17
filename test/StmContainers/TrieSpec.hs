{-# language ViewPatterns #-}
module StmContainers.TrieSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import StmContainers.Trie.Internal ()
import qualified StmContainers.Trie as Trie
import Control.Concurrent.STM
import Control.Monad
import Test.QuickCheck

spec :: Spec
spec = do
    describe "lookup" do
        prop "works after insert" \(NonEmpty path) -> do
            mr <- atomically do
                t <- Trie.new @String @Char
                Trie.insert path 'a' t
                Trie.lookup path t
            mr `shouldBe` Just 'a'

        prop "returns Nothing after a delete, not impacting children" \(NonEmpty path) extra -> do
            mr <- atomically do
                t <- Trie.new @String @Char
                Trie.insert path 'a' t
                let pathPlus = path <> [extra]
                Trie.insert (path <> [extra]) 'a' t
                Trie.delete path t
                (,) <$> Trie.lookup path t <*> Trie.lookup pathPlus t
            mr `shouldBe` (Nothing, Just 'a')

    describe "insert" do
        it "works" do
            t <- Trie.newIO
            atomically do
                Trie.insert ["goodbye"] 1 t
                Trie.insert ["hello", "world"] 3 t
                Trie.insert ["hello", "yes"] 1 t
                Trie.insert ["hello", "yes"] 2 t
            xs <- Trie.toListNonAtomic t
            xs `shouldMatchList`
                [ ( ["hello", "world"]
                  , (3 :: Int)
                  )
                , ( ["hello", "yes"]
                  , (2 :: Int)
                  )
                , ( ["goodbye"]
                  , 1
                  )
                ]
            forM_ xs \(ks, v) -> do
                mv' <- atomically do
                    Trie.lookup ks t
                mv' `shouldBe` Just v

    describe "null" do
        it "with empty map" do
            t <- Trie.newIO
            r <- atomically do
                Trie.null t
            r `shouldBe` True

        it "with nokey map" do
            t <- atomically do
                t <- Trie.new :: STM (Trie.Trie String Char)
                Trie.insert [] 'a' t
                Trie.null t
            t `shouldBe` False

        it "with full map" do
            t <- atomically do
                t <- Trie.new
                Trie.insert (replicate 10 "hello") 'a' t
                Trie.insert ["hello", "world"] 'a' t
                Trie.null t
            t `shouldBe` False

        it "after some deletions" do
            r <- atomically do
                t <- Trie.new
                Trie.insert ["hello", "world"] 'a' t
                Trie.delete ["hello", "world"] t
                Trie.null t
            r `shouldBe` True

        prop "insert then delete always null" \(NonEmpty xs) -> do
            r <- atomically do
                t <- Trie.new @String @Char
                Trie.insert xs 'a' t
                Trie.delete xs t
                Trie.null t
            r `shouldBe` True


    describe "delete" do
        it "works" do
            xs <- atomically do
                t <- Trie.new
                Trie.insert ["hello", "world"] 'a' t
                Trie.insert ["hello", "goodbye"] 'b' t
                Trie.delete ["hello"] t
                Trie.delete ["hello", "world"] t
                Trie.toList t
            xs `shouldMatchList`
                [ ( ["hello", "goodbye"]
                  , 'b'
                  )
                ]

    describe "deleteChildren" do
        it "works" do
            xs <- atomically do
                t <- Trie.new
                Trie.insert [] 'c' t
                Trie.insert ["hello", "world"] 'a' t
                Trie.insert ["hello", "goodbye"] 'b' t
                Trie.deleteChildren ["hello"] t
                Trie.delete ["hello", "world"] t
                Trie.toList @[] t
            xs `shouldMatchList` [([], 'c')]
