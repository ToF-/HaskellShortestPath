import Test.Hspec
import ShortestPath

data City = A | B | C | D
    deriving (Eq, Ord, Show)

main = hspec $ do
    describe "a graph" $ do
        it "allows for adjacent nodes to a node" $ do
            let g = fromList [(A,3,B),(B,2,C),(A,4,C),(C,1,D)]
            adjacentNodes A g  `shouldBe` [(B,3),(C,4)]
            adjacentNodes B g  `shouldBe` [(A,3),(C,2)]
