module Exercicio1F1 where

type Ponto = (Double,Double)
--a)
perimetro::Float->Float
perimetro x = 2*pi*x

--b)
dist::Ponto->Ponto->Double
dist a b = sqrt( (((fst$b) - (fst$a))^2) + (((snd$b) - (snd$a))^2))


--c)
primUlt::[Int]->Maybe (Int,Int)
primUlt [] = Nothing
primUlt l = Just (head l, last l)


--d)
multiplo::Int->Int->Bool
multiplo x y | ((mod x y) == 0) = True
             | otherwise = False

multiplo2::Int->Int->Bool
multiplo2 x y = ((mod x y) == 0)


--e)
truncaImpar::Eq a => [a]->[a]
truncaImpar l | ((mod (length l) 2)==0) = l            
              | otherwise = drop 1 l


--f)
max2::Int->Int->Int
max2 x y | (x<y) = y
         | otherwise = x


--g)
max3::Int->Int->Int->Int
max3 x y z = max2 (max2 x y) z 