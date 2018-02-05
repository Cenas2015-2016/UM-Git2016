module Exercicio4F2 where

--a)
segundos::[(a,b)]->[b]
segundos [] = []
segundos l@(x:xs) = (snd x):segundos xs


--b)
nosPrimeiros::(Eq a)=> a->[(a,b)]->Bool
nosPrimeiros _ [] = False
nosPrimeiros x l@(y:ys) | (x==(fst$y)) = True
                        | otherwise = nosPrimeiros x ys


--c)
menor::(Ord a) => a->[(a,b)]->a
menor min [] = min
menor min l@((a,b):xs) | (a<min) = menor a xs
                       | otherwise = menor min xs

minFst::(Ord a) => [(a,b)]->a
minFst l@(x:xs) = menor (fst$x) l 


--d)
sndMinFst::(Ord a) => [(a,b)]->b--MUITO INEFICIENTE
sndMinFst l@(x:xs) | ((fst$x) == minFst l) = snd$x
                   |otherwise = sndMinFst xs


--e)
soma::(Num a,Num b,Num c) => a->b->c->[(a,b,c)]->(a,b,c)
soma a b c [] = (a,b,c)
soma a b c l@((x,y,z):xs) = soma (a+x) (b+y) (c+z) xs

sumTriplos::(Num a, Num b, Num c) => [(a,b,c)]->(a,b,c)
sumTriplos l@((a,b,c):xs) = soma a b c xs


--f)
myMax::(Ord a,Num a) => (a,a,a) -> a
myMax (x,y,z) | (x<y) && (y<z) = z
            | (x<y) && (y>z) = y
            | otherwise = x

maxTriplo::(Ord a,Num a) => [(a,a,a)]->a
maxTriplo l = myMax (sumTriplos l)


