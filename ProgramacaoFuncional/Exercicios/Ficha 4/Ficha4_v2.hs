--Exercicio 2
type TabTemp = [(Data,Temp,Temp)] -- (data, temp. minima, temp. maxima)
type Data = (Int,Int,Int) -- (ano, mes, dia)
type Temp = Float
a = [((12,3,2012),13,15),((14,3,2012),17,25),((12,7,2012),23,35),((12,12,2012),0,1)]
--a)
medias :: TabTemp -> [(Data,Temp)]
medias [] = []

medias ((d,t1,t2):xs) = (d,((t1+t2)/2)):medias xs
{-com recurso a ordem superior (FUNCIONAL)

medias l = map(\(x,y,z)->(x,((y+z)/2))) l
-}

--b)
decrescente :: TabTemp -> Bool
decrescente [] = True
decrescente l@((d,t1,t2):xs) | ((compara d xs)==True) = decrescente xs
                             | otherwise = False


compara :: Data->TabTemp->Bool
compara (a,m,d) [] = True
compara (a,m,d) (((a1,m1,d1),t1,t2):xs) | (((a<=a1) && (m<=m1) && (d<=d1))==True) = compara (a,m,d) xs
                                        | otherwise = False


--c)
conta :: [Data] -> TabTemp -> Int
conta [] l = 0
conta ((a,m,d):xs) l | ((dataIgual (a,m,d) l)==True) = 1 + conta xs l
                     | otherwise = conta xs l  

dataIgual::Data->TabTemp->Bool
dataIgual (a,m,d) [] = False
dataIgual (a,m,d) (((a1,m1,d1),t1,t2):xs) | (((a1==a)&&(m1==m)&&(d1==d))==False) = dataIgual (a,m,d) xs 
                                          | otherwise = True


--Exercico 3
type MSet a = [(a,Int)]

--a)
--NAO SE ENCONTRA FUNCIONAL!!!
union :: Eq a => MSet a -> MSet a -> MSet a
union l1 l2 = listaTuplos(junta l1 l2)


--junta as duas listas numa só e replica os tuplos numa lista apenas
junta::Eq a => MSet a -> MSet a -> [a]
junta [] [] = []
junta ((a,b):xs) ((c,d):ys) = (replicate b a ++ replicate d c) ++ junta xs ys

--quanta quantos elementos iguais existem aquele 1º elemento do tuplo
contaElem :: Eq a =>a->[a]->Int
contaElem x [] = 0
contaElem x (a:xs) | (a==x) = 1 + contaElem x xs
                   | otherwise = contaElem x xs 

--elimina iguais na lista 
eliminaIguais ::Eq a => a -> [a] -> [a]
eliminaIguais x [] = []
eliminaIguais x (a:xs) | (a==x) = eliminaIguais x xs
                       | otherwise = a:eliminaIguais x xs

listaTuplos::Eq a=>[a]->[(a,Int)]
listaTuplos [] = []
listaTuplos l@(h:t) = (h,contaElem h l) : listaTuplos (eliminaIguais h l)



--b)
intersect :: Eq a => MSet a -> MSet a -> MSet a
intersect [] l2 = []
intersect l1 [] = []
intersect l1@((x,y):xs) l2@((a,b):ys) = iguais (x,y) l2 ++ intersect xs l2

iguais:: Eq a =>(a,Int)->MSet a -> MSet a
iguais (x,y) [] = []
iguais (x,y) l2@((a,b):ys) | (x==a) = [(x, min y b)]
                           | otherwise = iguais (x,y) ys


--c)
diff :: Eq a => MSet a -> MSet a -> MSet a
diff [] l2 = []
diff l1@((a,b):xs) l2@((c,d):ys) = (iguais2 (a,b) l2):diff xs l2
                                  
iguais2 :: Eq a =>(a,Int) -> MSet a -> (a,Int)
iguais2 (a,b) [] = (a,b)
iguais2 (a,b) ((x,y):xs) | (a==x) = (a,(b-y))
                         | otherwise = iguais2 (a,b) xs



--d) --Algoritmo do quickSort em MSets
ordena :: MSet a -> MSet a
ordena [] = []
ordena l@((x,y):xs) = let (menores,maiores) = parte (x,y) xs
                      in (ordena menores) ++ [(x,y)] ++ (ordena maiores)

parte:: (a,Int) -> MSet a -> (MSet a,MSet a)
parte (a,b) [] = ([],[])
parte (a,b) l@((x,y):xs) = let (menores, maiores) = parte (a,b) xs
                            in if (y<=b) then ((x,y):menores,maiores)
                                         else (menores,(x,y):maiores)


--algoritmo do InsertionSort em MSets
ordena2 :: MSet a -> MSet a
ordena2 [] = [] 
ordena2 (x:xs) = ins x (ordena xs) 
               where ins x [] = [x] 
                     ins x (b:bs) | (snd x <= snd b) = x : b : bs 
                                  | otherwise = b : (ins x bs) 



--e)
moda :: MSet a -> [a]
moda l@((a,b):xs) = map (fst) pares
                    where pares = filter (\(x,y) -> y == maior) l
                          maior = snd (maisRep (head l) l)
                          --maior = maximum (map (snd) l)
                          --usando a função maior que  está comentada nao tenho necessidade de usar a função "cenas" e "maisRep"

cenas :: (a,Int) -> MSet a -> [a] 
cenas (a,b) [] = []
cenas (a,b) ((c,d):xs) | (d==b) = c:cenas (a,b) xs
                       | otherwise = cenas (a,b) xs 

--Para descobrir um dos tuplos com mais repetições
maisRep :: (a,Int) -> MSet a -> (a,Int)
maisRep (a, b) [] = (a,b)
maisRep (a,b) ((x,y):xs) | (b<=y) = maisRep (x,y) xs
                         | otherwise = maisRep (a,b) xs

