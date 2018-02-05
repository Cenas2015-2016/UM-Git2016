module Ficha4Exe3 where 
import Data.Maybe
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show


type Nome = String
type Agenda = [(Nome, [Contacto])]


nums1 = [Casa 99990, Trab 88880, Tlm 87373, Email "lol@gmail.com"]
nums2 = [Casa 11110, Trab 84440, Tlm 87373, Email "lol2@gmail.com",Email "mania2@gmail.com",Email "mania@gmail.com"]
nums3 = [Casa 33450, Trab 77770, Tlm 87373, Email "lol3@gmail.com"]
nums4 = [Casa 99990, Trab 32100, Tlm 87373, Email "lol4@gmail.com"]
nums5 = [Casa 99990, Tlm 33333, Email "lel@gmail.com"]
nums6 = [Casa 99990, Tlm 22222, Email "lel2@gmail.com"]
nums7 = [Casa 99990, Tlm 22244, Email "lel3@gmail.com"]
nums8 = [Trab 88880, Email "yota9@gmail.com"]
nums9 = [Trab 88880, Email "yota8@gmail.com"]
nums10 = [Trab 88880, Email "yota7@gmail.com"]
nums11 = [Casa 99990, Email "toyota@gmail.com"]
nums12 = [Casa 99990, Email "toyota77@gmail.com"]
nums13 = [Email "toyota77@gmail.com"]
nums14 = [Email "toyota432@gmail.com"]
nums15 = [Email "toyota8604@gmail.com"]





lstAgenda =  [("Rafael", nums1),
              ("Miguel",nums2),
              ("Rogerio",nums3),
              ("Rodrigo",nums4),
              ("Flavia",nums5),
              ("Ana",nums6),
              ("Ines",nums7),
              ("Ze Luis",nums8),
              ("Cardante",nums9),
              ("Paulo",nums10),
              ("Cristovao",nums11),
              ("Igor",nums12),
              ("Eunice",nums13),
              ("Leonor",nums14),
              ("Fernando",nums15)]






acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n em [] = ((n,[Email em]):[])
acrescEmail n em ag@((nome,c@(y:ys)):xs) | (n==nome) = [(nome,((Email em):c))]
                                         | otherwise = (nome,c):acrescEmail n em xs  

getMails::[Contacto]->[String]
getMails [] = []
getMails ((Email x):xs) = (x:getMails xs)
getMails (x:xs) = getMails xs 


verEmails::Nome->Agenda->Maybe[String]
verEmails n [] = Nothing
verEmails n ag@((nome,contacts@(y:ys)):xs) | (nome==n) = Just (getMails contacts)
                                           | otherwise = verEmails n xs


consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs contactos@((Tlm tl):xs) = tl:consTelefs xs
consTelefs contactos@((Trab tb):xs) = tb:consTelefs xs
consTelefs contactos@((Casa c):xs) = c:consTelefs xs
consTelefs contactos@(x:xs) = consTelefs xs 


getNumCasa::[Contacto]->Maybe Integer
getNumCasa [] = Nothing
getNumCasa ((Casa x):xs) = Just x
getNumCasa (x:xs) = getNumCasa xs

casa :: Nome -> Agenda -> Maybe Integer
casa nome [] = Nothing
casa nome ag@((n,contacts):xs) | (n==nome) = getNumCasa contacts
                               | otherwise = casa nome xs



