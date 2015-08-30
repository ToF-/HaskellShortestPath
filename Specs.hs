import Test.Hspec
import Sample
import ShortestPath
import Data.PSQueue

rs = Graph roads


main = hspec $ do
    describe "shortest path" $ do
        it "from a node to itself is 0" $ do
            shortestPath rs Blaxhall Blaxhall `shouldBe` Path [(Blaxhall,0)] 
            shortestPath rs Clacton Clacton `shouldBe` Path [(Clacton,0)] 

        it "from a node to a connected node is the weigh" $ do
            shortestPath rs Blaxhall Dunwich `shouldBe` Path [(Dunwich,15)]

    describe "initial distances" $ do
        it "from a node yields all infinity except node" $ do
            toList (initialDistances rs Blaxhall) `shouldBe`
                [Blaxhall :-> 0
                ,Clacton :-> 10000000
                ,Dunwich :-> 10000000
                ,Feering :-> 10000000
                ,Harwich :-> 10000000
                ,Maldon :-> 10000000
                ,Tiptree :-> 10000000]

    describe "update distance" $ do
        it "update the distance table for a node if lower" $ do
            let ds = initialDistances rs Blaxhall 
            toList (updateDistance ds Dunwich 15)  `shouldBe` 
                [Blaxhall :-> 0
                ,Clacton :-> 10000000
                ,Dunwich :-> 15
                ,Feering :-> 10000000
                ,Harwich :-> 10000000
                ,Maldon  :-> 10000000
                ,Tiptree :-> 10000000]
        it "doesn't update the distance table for a node if higher" $ do
            let ds = initialDistances rs Blaxhall 
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
            let ds = initialDistances rs Blaxhall
                (ds',p) = shortestPathStep rs (ds,Path [])
            p  `shouldBe` Path [(Blaxhall,0)]        
        it "update the result for steps" $ do
            let ds = initialDistances rs Blaxhall
                (ds',p) = shortestPathStep rs (ds,Path [])
                (ds'',p') = shortestPathStep rs (ds',p)
            p'  `shouldBe`Path [(Blaxhall,0),(Dunwich,15)]
        it "update the result for steps" $ do
            let ds = initialDistances rs Blaxhall
                step = shortestPathStep rs
                (_,p) = step (step (step (step (step (step (step (ds,Path [])))))))
            p `shouldBe`Path  [(Blaxhall,0)
                         ,(Dunwich,15)
                         ,(Harwich,40)
                         ,(Feering,46)
                         ,(Tiptree,49)
                         ,(Clacton,57)
                         ,(Maldon,57)]   

