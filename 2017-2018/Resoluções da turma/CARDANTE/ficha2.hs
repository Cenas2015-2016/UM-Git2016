module Ficha2 where

import Data.Char

-- |Função que calcula o dobro de cada elemento da lista.
dobros :: [Float] -> [Float]
dobros [] = []
dobros l = map (*2) l

-- |Função que conta o número de ocorrências de um determinado caracter numa lista.
numOcorre :: Char -> String -> Int
numOcorre c l = length (filter (== c) l)

-- |Função que testa se uma lista de inteiros só tem elementos positivos.
positivos :: [Int] -> Bool
positivos l = (length l == length (filter (>0) l)) 

-- |Função que seleciona inteiros positivos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos l = (filter (>=0) l) 

-- |Função que soma os inteiros negativos de uma lista.
somaNeg :: [Int] -> Int
somaNeg l = foldr (+) 0 (filter (<0) l)

-- |Função que retorna os 3 últimos elementos de uma lista.
tresUlt :: [a] -> [a]
tresUlt l | (length l < 3) = l
tresUlt l = reverse (take 3 (reverse l))

-- |Função que seleciona as primeiras componentes de uma lista de pares.
primeiros :: [(a,b)] -> [a]
primeiros l = map (fst) l

-- |Função que testa se um caracter é uma letra minúscula.
isLower' :: Char -> Bool
isLower' char = (ord char >= ord 'a') && (ord char <= ord 'z')

-- |Função que testa se um caracter é uma letra maiúscula.
isUpper' :: Char -> Bool
isUpper' char = (ord char >= ord 'A') && (ord char <= ord 'Z')

-- |Função que testa se um caracter é um algarismo.
isDigit' :: Char -> Bool
isDigit' char = (ord char >= ord '0') && (ord char <= ord '9')

-- |Função que testa se um caracter é uma letra.
isAlpha' :: Char -> Bool
isAlpha' char = (isLower' char) || (ord char >= ord 'A') && (ord char <= ord 'Z')

-- |Função que converte uma letra minúscula para uma maiúscula.
toUpper' :: Char -> Char
toUpper' char = chr (ord char - shift)
                where shift = (ord 'a' - ord 'A')

-- |Função que converte um inteiro num algarismo (char).
intToDigit' :: Int -> Char
intToDigit' x = chr (48 + x)

-- |Função que converte um algarismo (char) num inteiro.
digitToInt' :: Char -> Int
digitToInt' char | isDigit' char = ord char - ord '0' 

-- |Função que testa se o primeiro elemento de uma String é uma letra maiúscula.
primMai :: String -> Bool
primMai (h:t) = isUpper' h

-- |Função que testa se o segundo elemento de uma String é uma letra minúscula.
segMin :: String -> Bool
segMin (h:hs:t) = isLower' hs

-- |Função que seleciona os algarismos de uma lista de caracteres.
soDigitos :: [Char] -> [Char]
soDigitos l = filter (isDigit') l

-- |Função que seleciona as letras minúsculas de uma lista de caracteres.
minusculas :: [Char] -> Int
minusculas l = length (filter (isLower') l)

-- |Função que seleciona os algarismos de uma String e converte-os para uma lista de inteiros.
nums :: String -> [Int]
nums l = map (digitToInt') (filter (isDigit') l)
