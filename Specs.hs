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
    

