module ShortestPath
where
import Data.PSQueue (PSQ, adjust, fromList, Binding((:->)), minView, null)

type Edge  n w = ( n, w )
type Route n w = [(w,n)]
type Graph n w = [(n, [Edge n w])]
type Dist  n w = (w, Maybe n)
type Path  n w = [(n, Dist n w)]
type Queue n w = PSQ n (Dist n w)

shortestPath :: (Ord n, Ord w, Num w) => Graph n w -> n -> n -> Route n w
shortestPath g a z = pathTo (Just z) $ allDistances g a

pathTo :: (Ord n, Ord w, Num w) => Maybe n -> Path n w -> Route n w
pathTo Nothing     _ = []
pathTo (Just k) ps =  pathTo j ps ++ [(w,k)]
    where Just (w,j) = lookup k ps

allDistances :: (Ord n, Ord w, Num w) => Graph n w -> n -> Path n w
allDistances g k = snd (loop ((initial g k),[]))
    where
    loop (ds,p) | Data.PSQueue.null ds = (ds,p)
                | otherwise   = loop (distances g k (ds,p))

distances :: (Ord n, Ord w, Num w) => Graph n w -> n -> (Queue n w, Path n w) -> (Queue n w, Path n w)
distances g a (ds,p) = case minView ds of
    Nothing            -> (ds,p)
    Just (n :-> (w,m),ds') -> (step ds' (lookup n g) n w, (n,(w,m)):p)

step :: (Ord n, Ord w, Num w) => Queue n w -> Maybe [(n,w)] -> n -> w -> Queue n w
step ds Nothing _ _ = ds
step ds (Just adjs) n0 w0 = foldl update ds adjs
    where
    update ds (k,w) = adjust (min (w0+w,Just n0)) k ds 

initial :: (Ord n, Ord w, Num w) => Graph n w -> n -> Queue n w
initial g k = adjust (const (0,Nothing)) k
    (fromList (map (\(n,_) -> n :->(10000,Nothing)) g))

                             
