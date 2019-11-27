import Test.Hspec

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Bijective as BHM
import Data.Tuple (swap)

main :: IO ()
main = hspec $
  describe "Data.HashMap.Bijective" $ do
    let dirty = BHM.insert 10 30 (BHM.singleton 10 20 :: BiHashMap Int Int)
    describe "empty" $
      it "creates an empty map" $ do
        let bhm = BHM.empty :: BiHashMap Int Int
        BHM.size bhm `shouldBe` 0
        BHM.null bhm `shouldBe` True
    describe "singleton" $
      it "creates a map with exactly one value" $ do 
        let bhm = BHM.singleton "hello" "world"
        BHM.lookup "hello" bhm `shouldBe` Just "world"
        BHM.lookupR "world" bhm `shouldBe` Just "hello"
        BHM.null bhm `shouldBe` False
        BHM.size bhm `shouldBe` 1
    describe "null" $
      it "reports nullity of a map correctly" $ do
        let first  = BHM.empty :: BiHashMap Int Int
        let second = BHM.singleton 60 06 :: BiHashMap Int Int
        BHM.null first `shouldBe` True
        BHM.null second `shouldBe` False
    describe "size" $
      it "reports the size of maps correctly" $ do
        let first  = BHM.singleton "mirror" "rorrim"
        let second = BHM.empty :: BiHashMap Int Int
        BHM.size first `shouldBe` 1
        BHM.size second `shouldBe` 0
        HM.size (BHM.left dirty) `shouldBe` HM.size (BHM.right dirty)
    describe "insert" $ do
      it "correctly inserts an association into the map" $ do
        BHM.lookup 10 dirty `shouldBe` Just 30
        BHM.lookupR 30 dirty `shouldBe` Just 10
      it "doesn't leave lingering keys" $ do
        BHM.lookupR 20 dirty `shouldBe` Nothing
        BHM.valid dirty `shouldBe` True
    describe "delete" $
      it "doesn't taint the map" $ do
        let deleted = BHM.delete 10 dirty
        BHM.valid deleted `shouldBe` True
        BHM.lookupR 30 deleted `shouldBe` Nothing
        BHM.lookup 10 deleted `shouldBe` Nothing
        HM.size (BHM.left deleted) `shouldBe` 0
        HM.size (BHM.right deleted) `shouldBe` 0
    describe "deleteR" $
      it "also doesn't taint the map" $ do
        let deleted = BHM.deleteR 30 dirty
        BHM.valid deleted `shouldBe` True
        BHM.lookupR 30 deleted `shouldBe` Nothing
        BHM.lookup 10 deleted `shouldBe` Nothing
        HM.size (BHM.left deleted) `shouldBe` 0
        HM.size (BHM.right deleted) `shouldBe` 0
    describe "fromList" $ do
      it "makes a valid map" $ do
        let imported = BHM.fromList [(10, 20), (40, 50), (10, 30)] :: BiHashMap Int Int
        BHM.lookup 10 imported `shouldBe` Just 30
        BHM.lookupR 30 imported `shouldBe` Just 10
        BHM.lookupR 20 imported `shouldBe` Nothing
        BHM.lookup 40 imported `shouldBe` Just 50
        BHM.lookupR 40 imported `shouldBe` Nothing
        BHM.lookup 50 imported `shouldBe` Nothing
        BHM.lookupR 50 imported `shouldBe` Just 40        
    -- describe "update" $ do
    --   it "updates an element" $ do
    --     let bhm  = BHM.singleton "hello" "world"
    --     let bhm' = BHM.update (return . reverse) "hello" bhm
    --     BHM.lookup "hello" bhm' `shouldBe` Just "dlrow"
    --   it "fails properly" $ 1 `shouldBe` 1
