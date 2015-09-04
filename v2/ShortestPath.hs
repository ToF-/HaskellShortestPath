module ShortestPath
where
import Data.Maybe
import Data.PSQueue as Q (Binding((:->)), fromList, PSQ, adjust)

type Graph n = [(n,[(n,Integer)])]
type Queue n = PSQ n (Integer,Maybe n)
adjacentNodes n = fromJust . lookup n

pathTo n d = case fromJust (lookup n d) of
    (w,Nothing) -> [(w,n)]
    (w,Just m)  -> pathTo m d ++ [(w,n)]

initialDistances :: (Eq n, Ord n) => n -> Graph n -> Queue n
initialDistances n g = adjust (const (0,Nothing)) n (fromList (map infinite g))
    where
    infinite (n,_) = n :-> (10000,Nothing)
