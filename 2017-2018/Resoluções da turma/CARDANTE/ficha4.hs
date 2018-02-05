module Ficha4 where

type TabTemp = [(Data,Temp,Temp)]        -- (data, temp. min, temp. max)
type Data = (Int,Int,Int)                -- (ano, mês, dia)
type Temp = Float

type MSet a = [(a,Int)]

-- |Constrói lista com as temperaturas médias de cada dia.
medias :: TabTemp -> [(Data,Temp)]
medias l = map (\(x,y,z) -> (x,(y+z)/2)) l

-- |Testa se a tabela está ordenada por ordem decrescente de data.
decrescente :: TabTemp -> Bool
decrescente l = aux (map (fst2) l)
                where fst2 (a,b,c) = a
                      aux [] = True
                      aux [x] = True
                      aux (h:hs:t) | (h > hs) = aux (hs:t)
                                   | otherwise = False

-- |Dada uma lista de datas e a tabela de registo de temperaturas, conta quantas das datas da lista têm registo na tabela.
conta :: [Data] -> TabTemp -> Int
conta [] _ = 0
conta _ [] = 0
conta (h:t) l | elem h (map (fst2) l) = 1 + conta t l
              | otherwise = conta t l
              where fst2 (a,b,c) = a

-- |Calcula a união de dois multi-conjuntos.
union :: Eq a => MSet a -> MSet a -> MSet a
union m1 m2 = aux (convertMSet (m1++m2))
              where  
                convertMSet [] = []
                convertMSet l = concat (map (\(char,times) -> replicate times char) l)
                
                aux [] = []
                aux l@(h:t) = (h, quantos h l) : aux (filter (/=h) l)

                quantos a [] = 0
                quantos a (h:t) | (a == h) = 1 + quantos a t
                                | otherwise = quantos a t 

-- |Calcula a interseção de dois multi-conjuntos.
intersect :: Eq a => MSet a -> MSet a -> MSet a
intersect m1 m2 = concat (aux m1 m2)
                  where
                    aux l [] = [l]
                    aux [] l = []
                    aux m1@(h:t) m2@(x:xs) = upd h m2 : aux t m2 

-- |Função auxiliar capaz de atualizar um elemento da primeira lista, em função da segunda lista de input.
upd :: Eq a => (a, Int) -> MSet a -> MSet a
upd m [] = []
upd m@(char, times) ms@((char2,times2):t) 
          | (char == char2) = [(char, min times times2)]
          | otherwise = upd m t 


-- |Calcula a diferença de dois multi-conjuntos.
diff :: Eq a => MSet a -> MSet a -> MSet a
diff m1 m2 = concat (aux m1 m2)
             where 
               aux l [] = [l]
               aux [] l = []
               aux m1@(h:t) m2@(x:xs) = checkIf h m2 : aux t m2 

-- |Função auxiliar que verifica se (a primeira componente de) um elemento da primeira lista se encontra na segunda lista de input. 
checkIf :: Eq a => (a,Int) -> MSet a -> MSet a
checkIf m@(char,times) m2 | elem char k = upd2 m m2
                          | otherwise = [m]
                          where k = map (fst) m2

-- |Função auxiliar capaz de atualizar um elemento da primeira lista, em função da segunda lista de input.
upd2 :: Eq a => (a, Int) -> MSet a -> MSet a
upd2 m [] = []
upd2 m@(char, times) ms@((char2,times2):t) 
          | (char == char2) = [(char, count times times2)]
          | otherwise = upd2 m t
          where
            count x y | (x < y) = y - x
                      | otherwise = x - y

-- |Função que ordena um multi-conjunto pelo número crescente de ocorrências (Algoritmo Insertion Sort).
ordena :: MSet a -> MSet a
ordena [] = [] 
ordena (x:xs) = ins x (ordena xs) 
               where ins x [] = [x] 
                     ins x (b:bs) | (snd x <= snd b) = x : b : bs 
                                  | otherwise = b : (ins x bs) 

-- |Função que devolve a lista dos elementos com maior número de ocorrências.
moda :: MSet a -> [a]
moda [] = []
moda m = map (fst) l
         where l = filter (\(x,y) -> y == n) m
               n = snd (head (reverse (ordena m)))
