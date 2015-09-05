import Test.Hspec
import ShortestPath
import Data.PSQueue as Q (toList, Binding((:->)))

data City = A | B | C | D
    deriving (Eq, Ord, Show)


main = hspec $ do
    let g = fromList [(A,3,B),(B,2,C),(A,7,C),(C,1,D)]

    describe "a graph" $ do
        it "allows for adjacent nodes to a node" $ do
            adjacentNodes A g  `shouldBe` [(B,3),(C,7)]
            adjacentNodes B g  `shouldBe` [(A,3),(C,2)]

    describe "a distance queue" $ do
        it "is initialized with a graph for a given node" $ do
            let q = initialDistances A g
            Q.toList q `shouldBe` [A :-> (0, Nothing)
                                  ,B :-> (10000, Nothing)
                                  ,C :-> (10000, Nothing)
                                  ,D :-> (10000, Nothing)]

        it "is extracted with the closest node and updated" $ do
            let ndg = nextDistances g
                (q,l) = ndg (initialDistances A g,[])
            Q.toList q `shouldBe` [B :-> (3,Just A)
                                  ,C :-> (7,Just A)
                                  ,D :-> (10000, Nothing)]

            let (q,l) = ndg (ndg (initialDistances A g,[]))
            Q.toList q `shouldBe` [C :-> (5,Just B)
                                  ,D :-> (10000, Nothing)]

            let (q,l) = ndg (ndg (ndg (initialDistances A g,[])))
            Q.toList q `shouldBe` [D :-> (6, Just C)]

        it "can be calculated until the distance list is complete" $ do 
            allDistances A g `shouldBe` [(D,(6,Just C))
                                        ,(C,(5,Just B))
                                        ,(B,(3,Just A))
                                        ,(A,(0,Nothing))]
