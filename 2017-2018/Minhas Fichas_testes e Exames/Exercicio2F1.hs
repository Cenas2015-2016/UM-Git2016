module Exercicio2F1 where

nRaizes::Float->Float->Float->Maybe Int
nRaizes a b c | (((b^2)-4*a*c) == 0) = Just 1
              | (((b^2)-4*a*c) > 0) = Just 2
              | otherwise = Nothing


raizes::Float->Float->Float->[Float]
raizes a b c | ((nRaizes a b c) == Nothing) = []
             | ((nRaizes a b c) == Just 1) = [(-(b^2))/(2*a)]
             | otherwise = [(-b-(sqrt(b*b-4*a*c)))/(2*a), (-b+(sqrt(b*b-4*a*c)))/(2*a)] 
                                   

