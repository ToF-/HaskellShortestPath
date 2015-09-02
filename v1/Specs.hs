import Test.Hspec
import ShortestPath
import Data.PSQueue

data City = A | B | C | D | E deriving (Eq,Ord,Show)


main = hspec $ do
    let g = [(A,[(B,3),(C,5)])
            ,(B,[(A,3),(D,4)])
            ,(C,[(A,5)])
            ,(D,[(B,4),(E,2)])
            ,(E,[(D,2)])] :: Graph City Integer
    describe "list of distances from a node" $ do
        describe "allows for computing the path" $ do
            let dists = [(A,(0, Nothing))
                        ,(B,(3,Just A))
                        ,(C,(5,Just A))
                        ,(D,(7, Just B))
                        ,(E,(9,Just D))]
            
            it "to the same node" $ do
                pathTo A dists  `shouldBe` [(A,0)]

            it "to the next node" $ do
                pathTo B dists `shouldBe` [(A,0),(B,3)]

            it "to any node" $ do
                pathTo E dists  `shouldBe` [(A,0),(B,3),(D,7),(E,9)]

    describe "a priority queue for distances and a graph" $ do
        describe "allow for computing the next distance" $ do
            let q = fromList [A :-> (10000, Nothing)
                             ,B :-> (10000, Nothing) 
                             ,C :-> (0,     Nothing)
                             ,D :-> (10000, Nothing)
                             ,E :-> (10000, Nothing)]
                d = [] :: [Distance City Integer]

            it "when there is only one distance known" $ do
                let (q',d') = nextDistance g (q,d)
                d' `shouldBe` [(C,(0, Nothing))]
                toList q' `shouldBe`
                    [A :-> (5,Just C)
                    ,B :-> (10000,Nothing)
                    ,D :-> (10000,Nothing)
                    ,E :-> (10000,Nothing)]


            it "when there are several distances known" $ do
                let (q',d') = nextDistance g (nextDistance g (q,d))
                d' `shouldBe` [(A,(5, Just C)),(C,(0, Nothing))]
                toList q' `shouldBe`
                    [B :-> (8,Just A)
                    ,D :-> (10000,Nothing)
                    ,E :-> (10000,Nothing)]
    
    describe "a graph" $ do
        it "can be converted in a distance table from a node" $ do
            let d = allDistances g C
            d  `shouldBe` [(E,(14,Just D))
                          ,(D,(12,Just B))
                          ,(B,(8,Just A))
                          ,(A,(5,Just C))
                          ,(C,(0,Nothing))]

    describe "shortestPath" $ do
        it "finds the shortest path from a node to another" $ do
            shortestPath g A E `shouldBe` [(A,0),(B,3),(D,7),(E,9)]
            shortestPath g C E `shouldBe` [(C,0),(A,5),(B,8),(D,12),(E,14)]
            shortestPath g B C `shouldBe` [(B,0),(A,3),(C,8)]
        
        
   

