module ShortestPath
where
import Data.Maybe
import Data.List (sort, groupBy)
import Data.PSQueue as Q (PSQ, adjust, Binding ((:->)), insert, empty)

type Node a = a
type Adjacent a = (Node a, Integer)
type Graph a = [(Node a,[Adjacent a])]
type Queue a = Q.PSQ a (Integer,Maybe a)

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
