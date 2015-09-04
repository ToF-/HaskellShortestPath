module ShortestPath
where
import Data.Maybe
import Data.PSQueue as Q (Binding((:->)), fromList, PSQ, adjust)

data City = A|B|C|D deriving (Eq,Ord,Show)

adjacentNodes n = fromJust . lookup n

pathTo n d = case fromJust (lookup n d) of
    (w,Nothing) -> [(w,n)]
    (w,Just m)  -> pathTo m d ++ [(w,n)]

initialDistances :: City -> [(City,[(City,Integer)])] -> PSQ City (Integer,Maybe City)
initialDistances n g = adjust (const (0,Nothing)) n (fromList (map infinite g))
    where
    infinite (n,_) = n :-> (10000,Nothing)
