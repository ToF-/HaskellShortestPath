import Test.Hspec
import Sample
import ShortestPath

main = hspec $ do
    describe "shortest path" $ do
        it "from a node to itself is 0" $ do
            shortestPath roads Blaxhall Blaxhall `shouldBe` [(Blaxhall,0)] 
            shortestPath roads Clacton Clacton `shouldBe` [(Clacton,0)] 
