module ShortestPath
where
import Data.PSQueue (PSQ, adjust, fromList, Binding((:->)), minView)

type Graph k w = [(k,[(k,w)])]
type Path  k w = [(k,(w,k))]
type Dists k w = PSQ k (w,k)

shortestPath :: (Ord k, Ord w, Num w) => Graph k w -> k -> k -> Path k w
shortestPath _ a z | a == z = [(z,(0,a))]
shortestPath g a z = case lookup a g >>= lookup z of
    Nothing -> []
    Just w  -> [(z,(w,a))]

distances :: (Ord k, Ord w, Num w) => Graph k w -> k -> (Dists k w, Path k w) -> (Dists k w, Path k w)
distances g k (_,[]) = (initial g k, [])

initial :: (Ord k, Ord w, Num w) => Graph k w -> k -> Dists k w
initial g k = adjust (const (0,k)) k
    (fromList (map (\(n,_) -> n :->(10000,n)) g))

-- data Step k w = Step k w k deriving (Eq,Show)
-- data Path k w = Path [Step k w] deriving (Eq,Show)
-- 
-- type Dists k w = PSQ k w
-- 
-- shortestPath :: (Num w,Ord k, Ord w) => Graph k w -> k -> k -> Path k w
-- shortestPath g f t =
--     let ds = initialDistances g f
--         loop g (ds,p@(Path (Step x _ _:ns))) | x == t = (ds,p)
--         loop g r = loop g $ shortestPathStep g r
--         (_,Path p) = loop g (ds, Path []) 
--     in Path $ reverse p 
-- 
-- initialDistances :: (Ord k, Num w,Ord w) => Graph k w -> k -> Dists k w
-- initialDistances (Graph g) k = adjust (const 0) k (fromList (map (\(n,_) -> n :-> 10000000) g))
-- 
-- updateDistance :: (Ord k, Ord w) => Dists k w  -> k -> w -> Dists k w
-- updateDistance ds k w = adjust (min w) k ds
-- 
-- shortestPathStep :: (Ord w, Num w, Ord k) => Graph k w -> (Dists k w, Path k w) -> (Dists k w, Path k w) 
-- shortestPathStep (Graph g) (ds,Path p) = case minView ds of
--     Nothing -> (ds,Path p)
--     Just (k :-> w, ds') -> let adj = lookup k g
--                                update ds (n,d) = updateDistance ds n (w+d)
--                                ds''= case adj of
--                                    Just ks -> foldl update ds' ks
--                                    Nothing -> ds'
--                            in (ds'', Path (Step k w k:p)) 
-- 
--                             
