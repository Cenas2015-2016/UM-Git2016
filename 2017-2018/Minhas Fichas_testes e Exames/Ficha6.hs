module Ficha6 where

type Mat a = [[a]]

matriz1 = [[1,2,3],[4,3,2],[1,2,3]]
matriz2 = [[1,3],[4,3,2],[1,2,3]]
matriz3 = [[0,2,3],[0,0,2],[0,0,0]]
matriz4 = [[1,1,1],[1,1,1],[1,1,1]]
matriz5 = [[1,2,3],[4,3,2],[1,2,3],[1,1,1],[1,1,1],[1,1,1]]


--Exercicio 3
--a)
aux::Int->Mat a->Bool
aux _ [] = True
aux t mat@(x:xs) | ((length x)==t) = aux t xs
                 | otherwise = False

dimOk::Mat a -> Bool
dimOk [[]] = True
dimOk mat@(l:ls) = aux (length l) mat



--b)
dimMat::Mat a -> (Int,Int)
dimMat [[]] = (0,0)
dimMat mat@(x:xs) | (dimOk mat) = (length mat,length x)
                  | otherwise = (-1,-1)


--c)
somaLinhas::Num a => [a]->[a]->[a]
somaLinhas [] [] = []
somaLinhas l1@(x:xs) l2@(y:ys) = (x+y):somaLinhas xs ys

addMat::Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat mat1@((x:xs):ys) mat2@((t:ts):zs) = [somaLinhas (x:xs) (t:ts)] ++ addMat ys zs 

--ou
addMat2::Num a => Mat a -> Mat a -> Mat a
addMat2 [] [] = []
addMat2  xs ys = zipWith (zipWith (+)) xs ys {--O zipWith interno aplica a soma aos elementos da 1º sublista
                                                O ZipWith externo aplica o processo anterior ás 
                                                seguintes listas 
                                                -Outra interpretação: 
                                                Junta as listas que contêm as somas das 
                                                sublistas(linhas)-}

--ou
--Aconcelhada
addMat3::Num a => Mat a -> Mat a -> Mat a
addMat3 (x:xs) (y:ys) = (zipWith (+) x y):(addMat3 ys xs)

--d)
--Aconcelhada
transpose2::Eq a => Mat a -> Mat a
transpose2 m | ((head m) == []) = []
             | otherwise = (map head m):(transpose2(map tail m))

--ou
transpose :: Mat a -> Mat a
transpose [] = []
transpose matriz@(x:xs) | ((length (head matriz))>1) = (map head matriz):(transpose (map tail matriz))
                        | otherwise = [map head matriz]
-- 'x' é a linha da matriz
-- 'xs' são as restantes linhas da matriz
-- a condição existente é para garantir que nao estoura o 'head' e o 'tail' quando 
-- resta apenas 1 elemento das linhas


--NAO FOI FEITA POR MIM
multMat::Num a=>Mat a->Mat a->Mat a
multMat [] _ = []
multMat m1 m2 = (map (\x -> (sum (zipWith (*) (head m1) x))) (transpose m2)) : (multMat (tail m1) m2)

