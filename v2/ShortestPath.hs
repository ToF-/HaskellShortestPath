module ShortestPath
where
import Data.Maybe
import Data.List (sort, groupBy)
import Data.PSQueue as Q (PSQ, adjust, Binding ((:->)), insert, empty, minView, null)

type Node a = a
type Adjacent a = (Node a, Integer)
type Graph a = [(Node a,[Adjacent a])]
type Distance a = (Integer, Maybe a)
type Queue a = Q.PSQ a (Distance a)
type DistanceTable a = [(Node a,Distance a)]

adjacentNodes :: Eq a => Node a -> Graph a -> [Adjacent a]
adjacentNodes n g = fromJust (lookup n g)

fromList :: (Eq a, Ord a) => [(Node a,Integer,Node a)] -> Graph a
fromList = map associate . groupBy (same fst) . sort . concat . map adjacents
    where
    adjacents :: (Node a,Integer,Node a) -> [(Node a,(Adjacent a))]
    adjacents (a,d,b) = [(a,(b,d)),(b,(a,d))]

    same :: Eq b => (a -> b) -> a -> a -> Bool
    same f x y = f x == f y 

    associate :: [(Node a,Adjacent a)] -> (Node a,[Adjacent a])
    associate xs = (fst (head xs),map snd xs)

initialDistances :: (Eq a, Ord a) => Node a -> Graph a -> Queue a
initialDistances a g = foldr insert Q.empty (map fst g)
    where
    insert x = Q.insert x (if x == a then 0 else 10000, Nothing)

nextDistances :: (Eq a, Ord a) => Graph a -> (Queue a,DistanceTable a) -> (Queue a,DistanceTable a)
nextDistances g (q,l) = case minView q of
    Nothing     -> (q,l)
    Just (n :-> (d,m),q') -> (foldl adjustDistance q' (adjacentNodes n g),(n,(d,m)):l)
        where
        adjustDistance q (n',d') = adjust (min (d+d',Just n)) n' q 
                        
allDistances :: (Eq a, Ord a) => Node a -> Graph a -> DistanceTable a
allDistances n g = snd (until (Q.null . fst) (nextDistances g) (initialDistances n g,[]))
