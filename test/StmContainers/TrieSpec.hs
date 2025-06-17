module StmContainers.TrieSpec where

import Test.Hspec
import StmContainers.Trie.Internal ()
import qualified StmContainers.Trie as Trie
import Control.Concurrent.STM
import Control.Monad

spec :: Spec
spec = do
    describe "insert" do
        it "works" do
            t <- Trie.newIO
            atomically do
                Trie.insert ["goodbye"] 1 t
                Trie.insert ["hello", "world"] 3 t
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
