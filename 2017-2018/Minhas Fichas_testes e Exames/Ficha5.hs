module Ficha5 where
import Data.Char


alineaA::Int->[Int]
alineaA n = [2^x | x <- [0..n]]

--alineaB::Int->[(Int,Int)]
--alineaB n = [(x,y) | x<-[1..n],y<-[n..1],x==y]

--alnieaC::Int->[[Int]]
--alineaC n = [[x] | x<-[1..n]]


--Exercicio 3 -- Tudo numa unica travessia
aux::String->(String,String)->(String,String)
aux [] (l1,l2) = (l1,l2)
aux str@(x:xs) (l1,l2) | (isAlpha x) = aux xs (x:l1,l2)
                       | (isDigit x) = aux xs (l1,x:l2)
                       | otherwise = aux xs (l1,l2) {--para o caso de conter simobolos-}


digitAlpha::String->(String,String)
digitAlpha str@(x:xs) = aux str ([],[]) 


--Exercicio 4
aux2::[Int]->(Int,Int,Int)->(Int,Int,Int)
aux2 [] (n,z,p) = (n,z,p)
aux2 l@(x:xs) (n,z,p) | (x<0) = aux2 xs (n+1,z,p)
                      | (x==0) = aux2 xs (n,z+1,p)
                      |otherwise = aux2 xs (n,z,p+1)

nzp::[Int]->(Int,Int,Int)
nzp l@(x:xs) = aux2 l (0,0,0)


--Exercicio 5
aux3::Integral a=> a->a->a->a
aux3 x y i|(x>=y) = aux3 (x-y) y (i+1)
          | otherwise = i

myDivMod::Integral a=>a->a->(a,a)
myDivMod x y = ((aux3 x y 0), x-(aux3 x y 0)*y)


