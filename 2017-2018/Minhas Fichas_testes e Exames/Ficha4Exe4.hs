module Ficha4Exe4 where

type Dia = Int
type Mes = Int
type Ano = Int
type Nome = String

data Data = D Dia Mes Ano
     deriving Show

type TabDN = [(Nome,Data)]

lstAniversarios = [("Rafael",D 15 05 2004),
                   ("Luis", D 12 09 2010),
                   ("Eureca", D 31 12 2007),
                   ("Ana Paula", D 26 12 2001),
                   ("Cristovao", D 15 02 2018)]



procura::Nome->TabDN->Maybe Data
procura nome [] = Nothing
procura nome lst@((n,(D d m a)):xs) | (n==nome) = Just (D d m a) 
                                    | otherwise = procura nome xs


-- ou 

procura2::Nome->TabDN->Maybe Data
procura2 nome [] = Nothing
procura2 nome lst@((n,dat@(D d m a)):xs) | (n==nome) = (Just dat)
                                         | otherwise = procura2 nome xs



calcula::Data->Data->Int
calcula (D da ma aa) (D dn mn an) | (an<=aa) = aa - an
                                  | otherwise = 0

idade::Data->Nome->TabDN->Maybe Int
idade atual nome [] = Nothing
idade atual nome l@((n,dat):xs) | (n==nome) = Just (calcula atual dat)
                                | otherwise = idade atual nome xs


anterior::Data->Data->Bool
anterior (D d m a) (D dd mm aa) | (a==aa) && (m==mm) && (d<dd) = True
                                | (a==aa) && (m<mm) = True
                                | (a<aa) = True
                                | otherwise = False


myIsort::(Nome,Data)->TabDN->TabDN
myIsort (n,d) [] = [(n,d)]
myIsort (n,d) l@((nn,dd):xs) | (anterior dd d) = (nn,dd):myIsort (n,d) xs
                             | otherwise =  (n,d):(nn,dd):xs

ordena::TabDN->TabDN
ordena [] = []
ordena l@(x:xs) = myIsort x (ordena xs) 
                                

aux::Data->TabDN->[(Nome,Int)]
aux d [] = []
aux dat@(D d m a) l@((n,dn):xs) = (n,(calcula dat dn)):(aux dat xs)


porIdade::Data->TabDN->[(Nome,Int)]
porIdade (D d m a) l = reverse$(aux (D d m a) (ordena l))