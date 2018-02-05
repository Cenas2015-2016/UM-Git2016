module Teste09JAN2017 where

import Data.Char

--EXE1
type MSet a = [(a,Int)]
--a)
cardMSet::MSet a ->Int
cardMSet [] = 0
cardMSet l@(x:xs) = (snd x) + cardMSet xs

--b)
moda::MSet a -> [a]
moda [] = []
moda l@(x:xs) = constroi(aux x l)
            where 
                aux::(a,Int)->MSet a -> (a,Int)
                aux (c,n) [] = (c,n)
                aux (c,n) (x:xs) | (n<=(snd x)) = aux (fst x, snd x) xs
                                 | otherwise = aux (c,n) xs
                constroi::(a,Int)->[a]
                constroi (c,0) = []
                constroi (c,n) = c:constroi(c,n-1)

--c)
converteMSet::MSet a -> [a]
converteMSet [] = []
converteMSet (x:xs) = (aux x)++converteMSet xs
                where
                    aux::(a,Int) -> [a]
                    aux (c,0) = []
                    aux (c,n) = c:aux (c,n-1)


--d)
addcopies::Eq a => MSet a -> a -> Int -> MSet a
addcopies [] c n = [(c,n)]
addcopies l@(x:xs) c n | ((fst x)==c) = [(fst x,(snd x)+n)] ++ xs
                       | otherwise = x:addcopies xs c n



data SReais = AA Double Double 
            | FF Double Double
            | AF Double Double 
            | FA Double Double
            | Uniao SReais SReais


--class Show where
--    Show::a->String
--instance Show a => Show (SReais a) where


myShow :: SReais -> String
myShow (AA x y) = "]" ++ myShow x ++ "," ++ myShow y ++ "["
myShow (FF x y) = "[" ++ myShow x ++ "," ++ myShow y ++ "]"
myShow (AF x y) = "]" ++ myShow x ++ "," ++ myShow y ++ "]"
myShow (FA x y) = "[" ++ myShow x ++ "," ++ myShow y ++ "["
myShow (Uniao x y) = "(" ++ myShow x ++ "U" ++ myShow y ++ ") "





data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

infixa::ExpInt->String
infixa (Const x) = [intToDigit x]
infixa (Simetrico x) = "-" ++ (infixa x)
infixa (Menos x y) = "("++ (infixa x) ++ " - " ++ (infixa y) ++ ")"
infixa (Mais x y) = "("++ (infixa x) ++ " + " ++ (infixa y) ++ ")"
infixa (Mult x y) = "("++ (infixa x) ++ " x " ++ (infixa y) ++ ")"
