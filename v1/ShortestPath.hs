module ShortestPath
where
import Data.PSQueue

type Distance n w = (n,(w,Maybe n))
type Graph n w = [(n,[(n,w)])]

pathTo :: Eq n => n -> [Distance n w] -> [(n,w)]
pathTo n dists = case Prelude.lookup n dists of 
    Nothing -> error "node not in distance list"
    Just (w,Nothing) -> [(n,w)]
    Just (w,Just m) -> pathTo m dists ++ [(n,w)]

nextDistance :: (Ord w, Ord n) => Graph n w -> (PSQ n (w,Maybe n),[Distance n w]) -> (PSQ n (w,Maybe n),[Distance n w])
nextDistance g (q,d) = case minView q of
    Nothing -> (q,d)
    Just (n :-> (w,m),q') -> (q',[(n,(w,m))])

