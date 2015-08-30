module ShortestPath
where
import Data.PSQueue

type WeighedGraph k w = [(k,[(k,w)])]
type Path k w = [(k,w)]

type Distances k w = PSQ k w

shortestPath :: Ord (k) => Ord (w) => Num w => WeighedGraph k w -> k -> k -> Path k w
shortestPath g f t | f == t    = [(f,0)]
                   | otherwise = case Prelude.lookup f g >>= Prelude.lookup t of
                        Just n -> [(t,n)]

initialDistances :: Ord k => Ord w => Num w => WeighedGraph k w -> k -> PSQ k w
initialDistances g k = adjust (const 0) k (fromList (map (\(n,_) -> n :-> 10000000) g))

updateDistance :: Ord k => Ord w => Num w => PSQ k w  -> k -> w -> PSQ k w
updateDistance ds k w = adjust (min w) k ds

shortestPathStep :: Ord k => Ord w => Num w => WeighedGraph k w -> (PSQ k w, Path k w) -> (PSQ k w, Path k w) 
shortestPathStep g (ds,p) = case minView ds of
    Nothing -> (ds,p)
    Just (k :-> w, ds') -> let adj = Prelude.lookup k g
                               update ds (n,d) = updateDistance ds n (w+d)
                               ds''= case adj of
                                   Just ks -> Prelude.foldl update ds' ks
                                   Nothing -> ds'
                           in (ds'', p++[(k,w)]) 

                            
