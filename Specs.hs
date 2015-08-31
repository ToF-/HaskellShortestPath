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
                `shouldBe` [(0,Blaxhall)]

        it "form a node to a neighbor is the weight of the edge" $ do
            shortestPath rs Blaxhall Dunwich
                `shouldBe` [(0,Blaxhall),(15,Dunwich)]

        it "form a node to a neighbor's enighbort is the cumulated weight of edges" $ do
            shortestPath rs Blaxhall Tiptree
                `shouldBe` [(0,Blaxhall),(46,Feering),(49,Tiptree)]

    describe "distances" $ do
        it "from start node to itself is 0" $ do
            let (dists,_) = (initial rs Blaxhall, [])
            head (toList dists) `shouldBe` (Blaxhall :-> (0,Blaxhall))

        it "from start node to neighbors is weight" $ do
            let (dists,p) = (distances rs Blaxhall (initial rs Blaxhall,[]))
            head p `shouldBe` (Blaxhall,(0,Blaxhall))
            toList dists `shouldBe` [Clacton :-> (10000,Clacton)
                                    ,Dunwich :-> (15,Blaxhall)
                                    ,Feering :-> (46,Blaxhall)
                                    ,Harwich :-> (40,Blaxhall)
                                    ,Maldon  :-> (10000,Maldon)
                                    ,Tiptree :-> (10000,Tiptree)]

        it "from start node to neighbors' neighbors is cumulated weight" $ do
            let (dists,p) = (distances rs Blaxhall (distances rs Blaxhall (initial rs Blaxhall,[])))
            head p `shouldBe`(Dunwich,(15,Blaxhall))
            toList dists `shouldBe` [Clacton :-> (10000,Clacton)
                                    ,Feering :-> (46,Blaxhall)
                                    ,Harwich :-> (40,Blaxhall)
                                    ,Maldon :-> (10000,Maldon)
                                    ,Tiptree :-> (10000,Tiptree)]

            let (dists,p) = (distances rs Blaxhall (distances rs Blaxhall (distances rs Blaxhall (initial rs Blaxhall,[]))))
            head p `shouldBe`(Harwich,(40,Blaxhall))
            toList dists `shouldBe` [Clacton :-> (57,Harwich)
                                    ,Feering :-> (46,Blaxhall)
                                    ,Maldon :-> (10000,Maldon)
                                    ,Tiptree :-> (71,Harwich)]


            let (dists,p) = (distances rs Blaxhall (distances rs Blaxhall (distances rs Blaxhall (distances rs Blaxhall (initial rs Blaxhall,[])))))
            head p `shouldBe` (Feering,(46,Blaxhall))
            toList dists `shouldBe` [Clacton :-> (57,Harwich)
                                    ,Maldon :-> (57,Feering)
                                    ,Tiptree :-> (49,Feering)]

            let p = allDistances rs Blaxhall 
            p  `shouldBe` [(Clacton,(57,Harwich))
                              ,(Maldon,(57,Feering))
                              ,(Tiptree,(49,Feering))
                              ,(Feering,(46,Blaxhall))
                              ,(Harwich,(40,Blaxhall))
                              ,(Dunwich,(15,Blaxhall))
                              ,(Blaxhall,(0,Blaxhall))]
