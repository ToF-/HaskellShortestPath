module ShortestPath
where
import Data.PSQueue

type Distance n w = (n,(w,Maybe n))
type Edge n w = (n,w)
type Graph n w = [(n,[(n,w)])]
type Table n w = (PSQ n (w,Maybe n),[Distance n w])

pathTo :: Eq n => n -> [Distance n w] -> [(n,w)]
pathTo n dists = case Prelude.lookup n dists of 
    Nothing -> error "node not in distance list"
    Just (w,Nothing) -> [(n,w)]
    Just (w,Just m) -> pathTo m dists ++ [(n,w)]

nextDistance :: (Ord w, Eq n, Ord n, Num w) 
 => Graph n w -> Table n w -> Table n w
nextDistance g (q,d) = case minView q of
    Nothing -> (q,d)
    Just (n :-> (w,m),q') -> (updateDistances q' g n w,(n,(w,m)):d)

updateDistances :: (Ord n, Ord w, Num w) => PSQ n (w,Maybe n) -> Graph n w -> n -> w -> PSQ n (w,Maybe n) 
updateDistances q g n w = case Prelude.lookup n g of
    Nothing -> q
    Just adjs -> Prelude.foldl (adjustDistance (n,w)) q adjs

adjustDistance :: (Ord n, Ord w, Num w) => 
    Edge n w -> PSQ n (w,Maybe n) -> Edge n w -> PSQ n (w,Maybe n)
adjustDistance (n0,w0) q (n1,w1) = adjust (min (w0+w1, Just n0)) n1 q

allDistances :: (Ord n,Num w,Ord w) 
 => Graph n w -> n -> [Distance n w]
allDistances g a = snd (allDistances' initialDistances)
    where
    allDistances' (q,d) | Data.PSQueue.null q    = (q,d) 
                        | otherwise = allDistances' (nextDistance g (q,d))

    initialDistances = (adjust (const (0, Nothing)) a
        (fromList (map (\(n,_) -> n :-> (10000,Nothing)) g)),[])


