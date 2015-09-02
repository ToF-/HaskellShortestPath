import Test.Hspec
import ShortestPath

data City = A | B | C | D | E deriving (Eq,Ord,Show)


main = hspec $ do
    describe "list of distances from a node" $ do
        describe "allows for computing the path" $ do
            let dists = [(A,(0, Nothing)),(B,(3,Just A)),(C,(5,Just A)),(D,(7, Just B)),(E,(9,Just D))]
            
            it "to the same node" $ do
                pathTo A dists  `shouldBe` [(A,0)]

            it "to the next node" $ do
                pathTo B dists `shouldBe` [(A,0),(B,3)]

        
            it "to any node" $ do
                pathTo E dists  `shouldBe` [(A,0),(B,3),(D,7),(E,9)]
