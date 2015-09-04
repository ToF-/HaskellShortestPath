module ShortestPath
where
import Data.Maybe
import Data.PSQueue as Q (Binding((:->)), fromList, PSQ, adjust, minView)

type Graph n = [(n,[(n,Integer)])]
type Queue n = PSQ n (Integer,Maybe n)
type Table n = (Queue n,[(n,(Integer,Maybe n))])

adjacentNodes n = fromJust . lookup n

pathTo n d = case fromJust (lookup n d) of
    (w,Nothing) -> [(w,n)]
    (w,Just m)  -> pathTo m d ++ [(w,n)]

initialDistances :: (Eq n, Ord n) => n -> Graph n -> Queue n
initialDistances n g = adjust (const (0,Nothing)) n (fromList (map infinite g))
    where
    infinite (n,_) = n :-> (10000,Nothing)

nextDistances :: (Eq n, Ord n) => Table n -> Table n
nextDistances (q,l) = case minView q of
    Nothing -> (q,l)
    Just (n :-> (w,m),q') -> (q',[(n,(w,m))])
