import Test.Hspec
import ShortestPath
import Data.PSQueue as Q (toList, Binding((:->)))

data City = A | B | C | D
    deriving (Eq, Ord, Show)


main = hspec $ do
    let g = fromList [(A,3,B),(B,2,C),(A,4,C),(C,1,D)]

    describe "a graph" $ do
        it "allows for adjacent nodes to a node" $ do
            adjacentNodes A g  `shouldBe` [(B,3),(C,4)]
            adjacentNodes B g  `shouldBe` [(A,3),(C,2)]

    describe "a distance queue" $ do
        it "is initialized with a graph for a given node" $ do
            let q = initialDistances A g
            Q.toList q `shouldBe` [A :-> (0, Nothing)
                                  ,B :-> (10000, Nothing)
                                  ,C :-> (10000, Nothing)
                                  ,D :-> (10000, Nothing)]
