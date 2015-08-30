module ShortestPath
where

type WeighedGraph k w = [(k,[(k,w)])]

shortestPath :: Ord k => Num w => WeighedGraph k w -> k -> k -> [(k,w)]
shortestPath _ f t = [(f,0)]
