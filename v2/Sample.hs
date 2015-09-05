
module Sample
where

data City = Blaxhall|Clacton|Dunwich|Feering|Harwich|Maldon|Tiptree
    deriving (Show,Eq,Ord)

roads = [(Blaxhall,15,Dunwich)
        ,(Blaxhall,40,Harwich)
        ,(Blaxhall,46,Feering)
        ,(Clacton,17,Harwich)
        ,(Clacton,40,Maldon)
        ,(Clacton,29,Tiptree)
        ,(Dunwich,53,Harwich)
        ,(Feering,11,Maldon)
        ,(Feering,3,Tiptree)
        ,(Harwich,31,Tiptree)
        ,(Maldon,8,Tiptree)]
    
