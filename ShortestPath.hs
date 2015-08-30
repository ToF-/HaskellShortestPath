module ShortestPath
where
import Data.PSQueue

data Graph k w = Graph [(k,[(k,w)])]
data Path k w = Path [(k,w)] deriving (Eq,Show)

type Distances k w = PSQ k w

shortestPath :: (Num w,Ord k) => Graph k w -> k -> k -> Path k w
shortestPath (Graph g) f t | f == t    = Path [(f,0)]
                   | otherwise = case Prelude.lookup f g >>= Prelude.lookup t of
                        Just n -> Path [(t,n)]

initialDistances :: (Ord k, Num w,Ord w) => Graph k w -> k -> PSQ k w
initialDistances (Graph g) k = adjust (const 0) k (fromList (map (\(n,_) -> n :-> 10000000) g))

updateDistance :: (Ord k, Ord w) => PSQ k w  -> k -> w -> PSQ k w
updateDistance ds k w = adjust (min w) k ds

shortestPathStep :: (Ord w, Num w, Ord k) => Graph k w -> (PSQ k w, Path k w) -> (PSQ k w, Path k w) 
shortestPathStep (Graph g) (ds,Path p) = case minView ds of
    Nothing -> (ds,Path p)
    Just (k :-> w, ds') -> let adj = Prelude.lookup k g
                               update ds (n,d) = updateDistance ds n (w+d)
                               ds''= case adj of
                                   Just ks -> Prelude.foldl update ds' ks
                                   Nothing -> ds'
                           in (ds'', Path (p++[(k,w)])) 

                            
