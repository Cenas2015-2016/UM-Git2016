data BTree a = Empty
             | Node a (BTree a) (BTree a)
    deriving (Show,Eq)

arv2 = (Node 11 (Node 7 (Node 3 Empty Empty)
                      (Node 2 (Node 10 Empty Empty) Empty)
                      )
                      (Node 13 (Node 12 Empty Empty)
                              (Node 15 Empty (Node 16 Empty Empty))
                              )
      )

--a)
altura :: (BTree a) -> Int
altura Empty = 0
altura (Node x esq dir) = 1 + max (altura esq) (altura dir)



--b)
contaNodos :: (BTree a) -> Int
contaNodos Empty = 0
contaNodos (Node x esq dir) = 1 + contaNodos esq + contaNodos dir


--c)
folhas :: (BTree a) -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x esq dir) = (folhas esq) + (folhas dir)


--d)
prune :: Eq a => Int -> (BTree a) -> (BTree a)
prune _ Empty = Empty
prune 0 arvore =  arvore
prune altura (Node x esq dir) = Node x (prune (altura-1) esq) (prune (altura-1) dir)
                              
