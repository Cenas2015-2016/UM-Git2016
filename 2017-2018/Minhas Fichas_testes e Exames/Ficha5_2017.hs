module Ficha5_2017 where
import Data.Char
--Listas por compreensão

--Exercicio 1
--a)
{--
    [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
    Devolve uma lista com os valores compreendidos entre 1 e 20 e que o seu resultado de dividir por 2 e 3 tenha resto zero
--}

--b)
{--
    [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
    Cria uma lista com os elementos compreendidos de 1 a 20 cuja sua divisao por 2 dê resto zero e devolve dessa lista apenas os elementos que quando divididos por 3 têm resto zero  
-}

--c)
{--
	[(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
	Devolve uma lista de tuplos cuja soma dos elementos desses tupulos estejam compreendidos entre os valores de 0 a 20 e que a soma dos dois elementos do tupulo seja igual a 30. 
-}

--d)
{--
    [sum [y | y <- [1..x], odd y] | x <- [1..10]]

    odd - An odd number is an integer of the form n=2k+1, where k is an integer.

-}


--Exercicio 2
--a)
funcA::Int->Int->[Int]
funcA x y = [2^x | x <-[x..y]]


--b)----------------------------------------------ESTÁ ERRADA!
funcB::[Int]->[(Int,Int)]
funcB l = [ (a,b) | a<-l,b<-reverse$l]


--c)
{--
funcC::[Int]->[[Int]]
funcC l = [ [x] | a<-[0..(length l)]]
-}


--d)


--e)





--Exercicio 3
myDigitAlpha::String->(String,String) --Faz duas travessias
myDigitAlpha l@(x:xs) = (filter (isDigit) l,filter (isAlpha) l) 


{--
myDigitAlpha2::String->(String,String)---nao funciona
myDigitAlpha2 l@(x:xs)  = let (l1,l2)
                          in if (isDigit x) then (x:l1,l2)
                          	                else (l1,x:l2)

--}



myDigitAlpha3::String->(String,String) --FAZ UMA SÓ TRAVESSIA DA LISTA
myDigitAlpha3 [] = ([],[])
myDigitAlpha3 l@(x:xs) = aux l [] []

aux::String->String->String->(String,String)
aux [] l1 l2 = (l1,l2)
aux l@(x:xs) l1 l2 | (isDigit x) = aux xs (x:l1) l2 
                   | otherwise = aux xs l1 (x:l2)



--Exercicio 4 --FAZ UMA UNICA TRAVESSIA
nzp::[Int]->(Int,Int,Int)
nzp l@(x:xs) = aux2 l (0,0,0)

aux2::[Int]->(Int,Int,Int)->(Int,Int,Int)
aux2 [] (n,z,p) = (n,z,p)
aux2 l@(x:xs) (n,z,p) | (x<0) = aux2 xs (n+1,z,p)
                      | (x==0) = aux2 xs (n,z+1,p)
                      | otherwise = aux2 xs (n,z,p+1)



--Exercicio 5
myDivMod::Integral a => a->a->(a,a) --A PARTE DECIMAL DÁ ERRADO
myDivMod x divisor = (pInteira x divisor 0,pDecimal x divisor)

pInteira::Integral a=>a->a->a->a
pInteira x divisor pInt |(x>=0) = pInteira (x-divisor) divisor (pInt+1) 
                        | otherwise = pInt

pDecimal::Integral a=>a->a->a
pDecimal x divisor = x-((pInteira x divisor 0)*divisor)




--Exercicio 6
