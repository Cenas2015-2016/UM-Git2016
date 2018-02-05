module Exercicio3F2 where
import Data.Char
--a)
soDIgitos::[Char] -> [Char]
soDIgitos [] = []
soDIgitos l@(x:xs) | (x>='0' && x<='9') = x:soDIgitos xs
                   | otherwise = soDIgitos xs

--b)
minusculas::[Char]->[Char]
minusculas [] = []
minusculas l@(x:xs) | (((ord x)>=97) && ((ord x)<=122)) = x:minusculas xs
                    | otherwise = minusculas xs

--c)
nums::String->[Int]
nums [] = []
nums l@(x:xs) | ((ord x)>=48) && ((ord x)<=57) = (digitToInt x):nums xs
              | otherwise = nums xs