module ShortestPath
where
import Data.PSQueue

type WeighedGraph k w = [(k,[(k,w)])]

type Distances k w = PSQ k w

shortestPath :: Ord (k) => Ord (w) => Num w => WeighedGraph k w -> k -> k -> [(k,w)]
shortestPath g f t | f == t    = [(f,0)]
                   | otherwise = case Prelude.lookup f g >>= Prelude.lookup t of
                        Just n -> [(t,n)]

initialDistances :: Ord k => Ord w => Num w => WeighedGraph k w -> k -> PSQ k w
initialDistances g k = adjust (const 0) k (fromList (map (\(n,_) -> n :-> 10000000) g))
