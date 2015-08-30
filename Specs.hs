import Test.Hspec
import Sample
import ShortestPath
import Data.PSQueue

main = hspec $ do
    describe "shortest path" $ do
        it "from a node to itself is 0" $ do
            shortestPath roads Blaxhall Blaxhall `shouldBe` [(Blaxhall,0)] 
            shortestPath roads Clacton Clacton `shouldBe` [(Clacton,0)] 

        it "from a node to a connected node is the weigh" $ do
            shortestPath roads Blaxhall Dunwich `shouldBe` [(Dunwich,15)]

    describe "initial distances" $ do
        it "from a node yields all infinity except node" $ do
            toList (initialDistances roads Blaxhall) `shouldBe`
                [Blaxhall :-> 0
                ,Clacton :-> 10000000
                ,Dunwich :-> 10000000
                ,Feering :-> 10000000
                ,Harwich :-> 10000000
                ,Maldon :-> 10000000
                ,Tiptree :-> 10000000]

    describe "update distance" $ do
        it "update the distance table for a node if lower" $ do
            let ds = initialDistances roads Blaxhall 
            toList (updateDistance ds Dunwich 15)  `shouldBe` 
                [Blaxhall :-> 0
                ,Clacton :-> 10000000
                ,Dunwich :-> 15
                ,Feering :-> 10000000
                ,Harwich :-> 10000000
                ,Maldon  :-> 10000000
                ,Tiptree :-> 10000000]
        it "doesn't update the distance table for a node if higher" $ do
            let ds = initialDistances roads Blaxhall 
            toList (updateDistance ds Blaxhall 15)  `shouldBe` 
                [Blaxhall :-> 0
                ,Clacton :-> 10000000
                ,Dunwich :-> 10000000
                ,Feering :-> 10000000
                ,Harwich :-> 10000000
                ,Maldon  :-> 10000000
                ,Tiptree :-> 10000000]
    describe "shortestPathStep" $ do
        it "update the result for a step" $ do
            let ds = initialDistances roads Blaxhall
                (ds',p) = shortestPathStep roads (ds,[])
            p  `shouldBe` [(Blaxhall,0)]        
        it "update the result for steps" $ do
            let ds = initialDistances roads Blaxhall
                (ds',p) = shortestPathStep roads (ds,[])
                (ds'',p') = shortestPathStep roads (ds',p)
            p'  `shouldBe`[(Blaxhall,0),(Dunwich,15)]
        it "update the result for steps" $ do
            let ds = initialDistances roads Blaxhall
                step = shortestPathStep roads
                (_,p) = step (step (step (step (step (step (step (ds,[])))))))
            p `shouldBe` [(Blaxhall,0)
                         ,(Dunwich,15)
                         ,(Harwich,40)
                         ,(Feering,46)
                         ,(Tiptree,49)
                         ,(Clacton,57)
                         ,(Maldon,57)]   

