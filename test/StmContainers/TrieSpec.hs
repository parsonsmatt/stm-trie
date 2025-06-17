{-# language ViewPatterns #-}
module StmContainers.TrieSpec where

import Control.Monad
import Data.Traversable
import Control.Monad.IO.Class
import qualified Data.Set as Set
import qualified Focus
import Data.Foldable
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Hedgehog
import StmContainers.Trie.Internal ()
import qualified StmContainers.Trie as Trie
import Control.Concurrent.STM
import Test.QuickCheck (NonEmptyList(..))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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

    describe "size" do
        it "works" $ hedgehog do
            ks <- forAll do
                Gen.set (Range.linear 1 100) do
                    Gen.list (Range.linear 1 10) do
                        Gen.string (Range.linear 1 30) Gen.unicode

            r <- liftIO $ atomically do
                t <- Trie.new @String
                for_ (Set.toList ks) \k -> do
                    Trie.insert k 'a' t
                Trie.size t

            r === length ks

        it "works with deletions" $ hedgehog do
            ks <- forAll do
                Gen.set (Range.linear 1 100) do
                    Gen.list (Range.linear 1 10) do
                        Gen.string (Range.linear 1 30) Gen.unicode

            keysToDelete <- forAll do
                i <- Gen.integral (Range.linear 1 10)
                fmap Set.fromList $ for [0.. i :: Int] \_ ->
                    Gen.element ks

            r <- liftIO $ atomically do
                t <- Trie.new @String
                for_ (Set.toList ks) \k -> do
                    Trie.insert k 'a' t
                for_ keysToDelete \k -> do
                    Trie.delete k t
                Trie.size t

            r === (length ks - length keysToDelete)

    describe "focus" do
        it "works" do
            r <- atomically do
                t <- Trie.new
                Trie.insert ["hello", "goodbye"] 'a' t
                Trie.insert ["hello", "goodbye", "ok"] 'a' t
                mr <- Trie.focus (Focus.lookup <* Focus.delete) ["hello", "goodbye"] t
                r <- Trie.lookup ["hello", "goodbye"] t
                r' <- Trie.lookup ["hello", "goodbye", "ok"] t
                pure do
                    r `shouldBe` Nothing
                    mr `shouldBe` Just 'a'
                    r' `shouldBe` Just 'a'
            r

