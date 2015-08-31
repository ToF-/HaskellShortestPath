import Test.Hspec
import Sample
import ShortestPath
import Data.PSQueue
import Data.List

rs = roads


main = hspec $ do
    describe "shortestPath" $ do
        it "from a node to itself is a one step path" $ do
            shortestPath rs Blaxhall Blaxhall 
                `shouldBe` [(Blaxhall,(0,Blaxhall))]

        it "form a node to a neighbor is the weight of the edge" $ do
            shortestPath rs Blaxhall Dunwich
                `shouldBe` [(Dunwich,(15,Blaxhall))]

    describe "distances" $ do
        it "from start node to itself is 0" $ do
            let (dists,_) = distances rs Blaxhall (empty,[])
            head (toList dists) `shouldBe` (Blaxhall :-> (0,Blaxhall))

        -- it "from start node to neighbors is weight" $ do
        --     (take 4 . sort . toList) (distances rs Blaxhall)
        --          `shouldBe` [(Blaxhall :-> (0, Blaxhall))
        --                     ,(Dunwich  :-> (15, Blaxhall))
        --                     ,(Feering  :-> (46, Blaxhall))
        --                     ,(Harwich  :-> (40, Blaxhall))]

