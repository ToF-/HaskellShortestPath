import Test.Hspec
import ShortestPath
import Data.PSQueue


main = hspec $ do
    let g = [(A,[(B,2),(C,3)])
            ,(B,[(A,2),(C,4)])
            ,(C,[(A,3),(B,4)])]
    describe "a graph" $ do
        it "contains nodes and their adjacent nodes" $ do
            adjacentNodes B g  `shouldBe` [(A,2),(C,4)]

    describe "a distance list" $ do
        it "allows for generating a path" $ do
            let d = [(A,(0, Nothing))
                    ,(B,(2, Just A))
                    ,(C,(6, Just B))]
            pathTo C d `shouldBe` [(0,A),(2,B),(6,C)]

    describe "a distance queue to a node" $ do
        it "contains initially infinite distances except node" $ do
            toList (initialDistances A g)
            `shouldBe` [A :-> (0, Nothing)
                       ,B :-> (10000, Nothing)
                       ,C :-> (10000, Nothing)]
                                
        


