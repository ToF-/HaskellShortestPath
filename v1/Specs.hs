import Test.Hspec
import ShortestPath

data City = A | B | C | D | E deriving (Eq,Ord,Show)


main = hspec $ do
    describe "list of distances from a node" $ do
        describe "allows for computing the path" $ do
            it "to the same node" $ do
                let dists = [(A,(0, Nothing)),(B,(3,Just A)),(C,(5,Just A)),(D,(7, Just B)),(E,(9,Just D))]
                pathTo A dists  `shouldBe` [(A,0)]
