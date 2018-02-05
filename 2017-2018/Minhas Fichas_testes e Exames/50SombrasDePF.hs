module 50SombrasDePF where

--1)
enumFromTo::Int->Int->[Int]
enumFromTo x y | (x<=y) = x:enumFromTo (x+1) y
               | otherwise = []