module ShortestPath
where

pathTo :: (Eq n) => n -> [(n,(w,Maybe n))] -> [(n,w)]
pathTo n dists = case lookup n dists of 
    Nothing -> error "node not in distance list"
    Just (w,_) -> [(n,w)]
