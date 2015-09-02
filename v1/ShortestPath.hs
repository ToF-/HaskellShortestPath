module ShortestPath
where

type Distance n w = (n,(w,Maybe n))

pathTo :: (Eq n) => n -> [Distance n w] -> [(n,w)]
pathTo n dists = case lookup n dists of 
    Nothing -> error "node not in distance list"
    Just (w,Nothing) -> [(n,w)]
    Just (w,Just m) -> pathTo m dists ++ [(n,w)]
