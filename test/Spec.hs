import Test.Hspec

import Data.HashMap.Bijective as BHM

main :: IO ()
main = hspec $
  describe "Data.HashMap.Bijective" $ do
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
    -- describe "update" $ do
    --   it "updates an element" $ do
    --     let bhm  = BHM.singleton "hello" "world"
    --     let bhm' = BHM.update (return . reverse) "hello" bhm
    --     BHM.lookup "hello" bhm' `shouldBe` Just "dlrow"
    --   it "fails properly" $ 1 `shouldBe` 1
