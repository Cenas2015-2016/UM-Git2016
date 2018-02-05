module Exercicio5F1 where

data Semaforo = Verde|Amarelo|Vermelho deriving (Show,Eq)

--a)

next::Semaforo->Semaforo
next cor | (cor==Verde) = Amarelo
         | (cor==Amarelo) = Vermelho
         | otherwise = Verde
    

stop::Semaforo->Bool
stop cor = (cor==Vermelho)        


safe::Semaforo->Semaforo->Bool
safe cor1 cor2 = (cor1/=Verde) || (cor2/=Verde)