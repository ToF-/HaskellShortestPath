import Test.Hspec
import Sample
import ShortestPath
import Data.PSQueue

rs = roads


main = hspec $ do
    describe "shortestPath" $ do
        it "from a node to itself is a one step path" $ do
            shortestPath rs Blaxhall Blaxhall 
                `shouldBe` [(Blaxhall,(0,Blaxhall))]
