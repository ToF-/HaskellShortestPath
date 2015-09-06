
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
    

grid =  [((0,0),1,(1,0))
        ,((0,0),1,(0,1))
        ,((1,0),1,(2,0))
        ,((1,0),1,(1,1))
        ,((2,0),1,(3,0))
        ,((2,0),1,(2,1))
        ,((3,0),1,(3,1))
        ,((0,1),1,(1,1))
        ,((0,1),1,(0,2))
        ,((1,1),1,(2,1))
        ,((1,1),1,(1,2))
        ,((2,1),1,(3,1))
        ,((2,1),1,(2,2))
        ,((3,1),1,(3,2))
        ,((0,2),1,(1,2))
        ,((0,2),1,(0,3))
        ,((1,2),1,(2,2))
        ,((1,2),1,(1,3))
        ,((2,2),1,(3,2))
        ,((2,2),1,(2,3))
        ,((3,2),1,(3,3))
        ,((0,3),1,(1,3))
        ,((1,3),1,(2,3))
        ,((2,3),1,(3,3))]
        
