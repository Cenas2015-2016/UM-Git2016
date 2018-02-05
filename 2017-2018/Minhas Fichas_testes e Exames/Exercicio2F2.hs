module Exercicio2F2 where

--a)
dobros::[Float]->[Float]
dobros [] = []
dobros l@(x:xs) = (x*2):dobros xs

--b)
numOcorre::Char->String->Int
numOcorre _ [] = 0
numOcorre char l@(x:xs) | (char==x) = 1 + numOcorre char xs
                        | otherwise = numOcorre char xs

--c)
positivos::[Int]->Bool
positivos [] = True
positivos l@(x:xs) | (x>=0) = positivos xs
                   | otherwise = False

--d)
soPos::[Int]->[Int]
soPos [] = []
soPos l@(x:xs) | (x>=0) = x:soPos xs
               | otherwise = soPos xs

--e)
somaNeg::[Int]->Int
somaNeg [] = 0
somaNeg l@(x:xs) | (x<0) = x + somaNeg xs
                 | otherwise = somaNeg xs


--f)
tresUlt::[a]->[a]
tresUlt [] = []
tresUlt (x:[]) = [x]
tresUlt (x:y:[]) = [x,y]
tresUlt (x:y:z:[]) = [x,y,z]
tresUlt l = drop ((length l)-3) l 

--g)
primeiros::[(a,b)]->[a]
primeiros [] = []
primeiros l@(x:xs) = (fst$x):primeiros xs


