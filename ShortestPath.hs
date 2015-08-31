module ShortestPath
where
import Data.PSQueue (PSQ, adjust, fromList, Binding((:->)), minView, null)

type Graph k w = [(k,[(k,w)])]
type Path  k w = [(k,(w,k))]
type Dists k w = PSQ k (w,k)

shortestPath :: (Ord k, Ord w, Num w) => Graph k w -> k -> k -> Path k w
shortestPath _ a z | a == z = [(z,(0,a))]
shortestPath g a z = case lookup a g >>= lookup z of
    Nothing -> []
    Just w  -> [(z,(w,a))]

allDistances :: (Ord k, Ord w, Num w) => Graph k w -> k -> Path k w
allDistances g k = snd (loop ((initial g k),[]))
    where
    loop (ds,p) | Data.PSQueue.null ds = (ds,p)
                | otherwise   = loop (distances g k (ds,p))

distances :: (Ord k, Ord w, Num w) => Graph k w -> k -> (Dists k w, Path k w) -> (Dists k w, Path k w)
distances g a (ds,p) = case minView ds of
    Nothing            -> (ds,p)
    Just (n :-> (w,m),ds') -> (step ds' (lookup n g) n w, (n,(w,m)):p)

step :: (Ord k, Ord w, Num w) => Dists k w -> Maybe [(k,w)] -> k -> w -> Dists k w
step ds Nothing _ _ = ds
step ds (Just adjs) n0 w0 = foldl update ds adjs
    where
    update ds (k,w) = adjust (min (w0+w,n0)) k ds 

initial :: (Ord k, Ord w, Num w) => Graph k w -> k -> Dists k w
initial g k = adjust (const (0,k)) k
    (fromList (map (\(n,_) -> n :->(10000,n)) g))

-                             
