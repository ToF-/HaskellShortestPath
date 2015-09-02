module ShortestPath
where

pathTo :: (Eq n) => n -> [(n,(w,Maybe n))] -> [(n,w)]
pathTo n dists = case lookup n dists of 
    Nothing -> error "node not in distance list"
    Just (w,Nothing) -> [(n,w)]
    Just (w,Just m) -> pathTo m dists ++ [(n,w)]
