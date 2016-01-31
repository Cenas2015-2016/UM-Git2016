module Ficha7 where

data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email String
    deriving Show

type Nome = String
type Agenda = [(Nome,[Contacto])]

minhaAgenda = (("Ricardo",(Casa 883456789,Trab 887654321,Tlm 886549876,"teste@hotmail.com")))
               --("Luis",(993456789,99764321,996549876,"tes@hotmail.com")),
               --("Vitor",(773456789,777654321,776549876,"mundo@hotmail.com")))
--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome,[Email email])]
acrescEmail nome email ((name,contactos):t) | (name==nome) = (nome,((Email email):contactos)):t
                                            | otherwise = acrescEmail nome email t

--b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome ((name,contactos):xs) | (name==nome) = Just (procuraEmails contactos)
                                     | otherwise = verEmails nome xs
                                        where
                                            procuraEmails [] = [] 
                                            procuraEmails ((Email email):restoContactos) = email:(procuraEmails restoContactos)
                                            procuraEmails (_:restoContactos) = procuraEmails restoContactos
--c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa num1):xs) = num1:consTelefs xs
consTelefs ((Trab num1):xs) = num1:consTelefs xs
consTelefs ((Tlm num1):xs) = num1:consTelefs xs
consTelefs (_:xs) = consTelefs xs

--d)
casa :: Nome -> Agenda -> Maybe Integer
casa nome ((name,contactos):xs) | (name == nome) = procuraTelCasa contactos
                                | otherwise = casa nome xs

procuraTelCasa :: [Contacto]->Maybe Integer
procuraTelCasa [] = Nothing
procuraTelCasa ((Casa num):xs) = Just num  
procuraTelCasa (_:xs) = procuraTelCasa xs



--Exercicio 2
type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String         <---Está comentado porque o tipo Nome já está definido no exercicio acima

data Data = D Dia Mes Ano
  deriving Show

type TabDN = [(Nome,Data)]

--a)
procura :: Nome -> TabDN -> Maybe Data
procura nome [] = Nothing
procura nome ((name,aniversario):xs) | (name == nome) = Just aniversario
                                     | otherwise = procura nome xs

--b)
idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d m a) nome [] = Nothing
idade (D d m a) nome ((name,aniversario):xs) | (name == nome) = Just (calcula (D d m a) aniversario)
                                             | otherwise = idade (D d m a) nome xs
calcula :: Data -> Data->Int
calcula (D d m a) (D dia mes ano) | (ano<a) && (mes<=m) = a-ano
                                  | otherwise = 0

--c)
anterior :: Data -> Data -> Bool--testa se a 1º data é anterior á segunda
anterior (D d m a) (D dd mm aa) | (a<aa) = True
                                | ((a==aa) && (m<mm)) = True
                                | ((a==aa) && (m==mm) && (d<dd)) = True
                                | otherwise =  False

--d)
ordena :: TabDN -> TabDN
ordena l@((nome,(D d m a)):xs) = (ordena (filter (\(_,(D x y z)) -> (((anterior (D x y z) (D d m a))==True))) xs))
                                                                    ++
                                                            [(nome,(D d m a))]
                                                                    ++
                                 (ordena (filter (\(_,(D x y z)) -> (((anterior (D x y z) (D d m a))==False))) xs))

--e)
porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade (D x y z) [] = []
porIdade (D x y z) l = ordenaTuplos(listaTuplos (D x y z) l)


listaTuplos :: Data -> TabDN -> [(Nome, Int)]
listaTuplos (D x y z) [] = []
listaTuplos (D x y z) ((nome,(D d m a)):xs) = (nome, (calcula (D x y z) (D d m a))):(listaTuplos (D x y z) xs)

ordenaTuplos :: [(Nome,Int)] -> [(Nome, Int)]
ordenaTuplos [] = []
ordenaTuplos l@((a,b):xs) = (ordenaTuplos (filter(\(x,y)->(y<=b)) xs)) ++ [(a,b)] ++ (ordenaTuplos (filter (\(x,y)->(y>b)) xs))
