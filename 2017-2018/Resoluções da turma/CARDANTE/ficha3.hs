module Ficha3 where 

import Ficha1

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- |Função que calcula a lista das segundas componentes dos pares.
segundos :: [(a,b)] -> [b]
segundos l = map (snd) l

-- |Função que testa se um elemento aparece na lista como primeira componente de algum dos pares.
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a l = elem a (map (fst) l)

-- |Função que seleciona a menor primeira componente de uma lista de pares.
minFst :: (Ord a) => [(a,b)] -> a
minFst l = foldr1 (\h t -> min h t) (map (fst) l)

-- |Função que calcula a segunda componente associada à menor primeira componente.
sndMinFst :: (Ord a) => [(a,b)] -> b
sndMinFst l = aux (minFst l) l
              where aux a ((c1,c2):t) | (a == c1) = c2
                                      | otherwise = aux a t

-- |Função que soma uma lista de triplos, componente a componente.
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos l = (foldr (+) 0 as, foldr (+) 0 bs, foldr (+) 0 cs) 
               where fst2 (a,b,c) = a
                     snd2 (a,b,c) = b
                     trd (a,b,c) = c
                     as = (map (fst2) l) 
                     bs = (map (snd2) l)
                     cs = (map (trd) l)
                     
-- |Função que calcula o máximo valor da soma das componentes de cada triplo de uma lista.                     
maxTriplo :: (Ord a, Num a) => [(a,a,a)] -> a
maxTriplo l = foldr1 (\h t -> max h t) (map (\(x,y,z) -> x+y+z) l)

-- |Função que multiplica dois números inteiros, por somas sucessivas.
(><) :: Int -> Int -> Int
(><) 0 y = 0
(><) x y = y + (><) (x-1) y

-- |Função que calcula a potência inteira de um número por multiplicações sucessivas.
power :: Integer -> Integer -> Integer --Integer porque permite uma maior potência.
power x 0 = 1
power x y = x * power x (y-1)

-- |Função que verifica se uma etapa tem horas válidas e ordenadas (isto é, hora de partida < hora de chegada).
-- Assume que uma etapa começa e acaba no mesmo dia.
verificaEtapa :: Etapa -> Bool
verificaEtapa (hp,hc) = verificaHora hp && verificaHora hc && compara hc hp

-- |Função que verifica a validade de uma viagem. Verifica se as etapas são válidas
-- (i.e., aplica verificaEtapa a todas as etapas da Viagem de input) e se cada etapa começa depois de a anterior ter terminado.
verificaViagem :: Viagem -> Bool
verificaViagem v = verificaConseq v && (not(elem False (map (verificaEtapa) v)))

-- |Função auxiliar que verifica se numa viagem, uma etapa só começa após a anterior ter terminado. 
verificaConseq :: Viagem -> Bool
verificaConseq [] = True
verificaConseq [h] = True
verificaConseq (h:hs:t) 
        | (hora1 < hora2) || ((hora1 == hora2) && (min1 < min2)) = verificaConseq (hs:t)
        | otherwise = False 
        where hora1 = (fst . snd) h
              hora2 = (fst . fst) hs
              min1 = (snd . snd) h
              min2 = (snd . fst) hs

-- |Função que calcula a hora de partida e de chegada de uma dada viagem.
intervaloViagem :: Viagem -> (Hora, Hora)
intervaloViagem [] = ((0,0),(0,0))
intervaloViagem v = (fst (head v), snd (last v))

-- |Função que calcula o tempo total de viagem efetiva.
tempoViagem :: Viagem -> Hora
tempoViagem v = convertePHora (foldr (+) 0 (map (\(h1,h2) -> diferenca h1 h2) v))

-- |Função que calcula o tempo total de espera (i.e., tempo que decorre entre as diferentes etapas).
tempoEspera :: Viagem -> Hora
tempoEspera v = convertePHora (aux v)
                where aux [] = 0
                      aux [x] = 0
                      aux (e1:e2:t) = diferenca (fst e2) (snd e1) + aux (e2:t)

-- |Função que calcula o tempo total de viagem (soma dos tempos de espera e de viagem efetiva).
tempoTotal :: Viagem -> Hora
tempoTotal v = convertePHora (convertePMin (tempoEspera v) + convertePMin (tempoViagem v))