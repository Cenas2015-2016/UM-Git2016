union' :: Eq a => [(a,Int)] -> [(a, Int)] -> [(a, Int)]
union' l l2 =  constroiMSet (converteMSet (l++l2)) 

constroiMSet::Eq a => [a]->[(a,Int)]
constroiMSet [] = []
constroiMSet list@(h:t) = (h, conta h list) : constroiMSet(deleteall h list)

conta::Eq a => a->[a]->Int
conta a [] = 0
conta a (h:t) |(a==h) = 1 + conta a t
              |otherwise = conta a t

converteMSet::[(a,Int)]->[a]
converteMSet [] = []
converteMSet (h:t) = replicate (snd h) (fst h) ++ converteMSet t

deleteall::Eq a => a->[a]->[a]
deleteall a [] = []
deleteall a (h:t) |(a==h) = deleteall a t
                  |otherwise = h : deleteall a t


type Mset a = [(a,Int)]

ordena :: Mset a -> Mset a
ordena [] = []
ordena l@((a,b):xs) = let (menores, maiores) = ordenador (a,b) xs
                      in (ordena menores) ++ [(a,b)] ++ (ordena maiores)

ordenador :: (a,Int) -> Mset a -> (Mset a, Mset a)
ordenador (a,b) [] = ([],[])
ordenador (a,b) ((c,d):xs) = let (menores, maiores) = ordenador (a,b) xs 
                             in if (d<=b) then ((c,d):menores,maiores)
                             	else (menores, (c,d):maiores)						   


--Começa por colocar o elemento com maior contagem após ordenada a lista de Mset's e depois aplica a função aux para colocar elementos
-- com igual contagem à do primeiro.
moda :: Mset a -> [a]
moda l = fst(head (reverse (ordena l))) : aux ((reverse(ordena l)))

--Função que cria uma lista com os primeiros elementos de cada par com contagem igual à do maior.
aux :: Mset a -> [a]
aux [] = []
aux [x] = []
aux (h:hs:t) |(snd hs == snd h) = (fst hs) : aux (h:t)
             |otherwise = aux (h:t)