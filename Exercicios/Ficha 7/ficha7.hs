module Ficha7 where

data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email String
    deriving Show

type Nome = String
type Agenda = [(Nome,[Contacto])]

minhaAgenda = (("Ricardo",(Casa, 883456789,Trab, 887654321,Tlm, 886549876,"teste@hotmail.com")))
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




--Exercicio 3
data Movimento = Credito Float 
                | Debito Float
  deriving Show

--data Data = D Int Int Int -- Dia Mes Ano
--  deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
  deriving Show

--na linha a baixo, um exemplo de extrato bancario para testes
extratoBanc = Ext 100029 [((D 12 10 2102), "repCarro", Credito 9999),((D 20 10 2102), "repCasa", Debito 9339),((D 1 1 2015), "Cenas", Debito 9340)]
--a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) valor = []
extValor (Ext saldo ((dat, descricao, Credito quantia):ys)) valor | (quantia>=valor) = (Credito quantia):extValor (Ext saldo ys) valor 
                                                                  | otherwise = extValor (Ext saldo ys) valor 

extValor (Ext saldo ((dat, descricao, Debito quantia):ys)) valor | (quantia>=valor) = (Debito quantia):extValor (Ext saldo ys) valor 
                                                                 | otherwise = extValor (Ext saldo ys) valor 



--b)--Nota: Por causa dos tipos, os termos de comparação para criar o tuplo tem que usar o 'elem', pois a simpes comparação com (==) não funciona.
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) descricao= []
filtro (Ext saldo ((dat,descr,movimento):xs)) descricao | (elem descr descricao) = (dat,movimento):(filtro (Ext saldo xs) descricao) 
                                                        | otherwise = filtro (Ext saldo xs) descricao

--c)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext saldo ((_, _, Credito montante):xs)) = let (totalC,totalD) = creDeb (Ext saldo xs) 
                                                   in (totalC + montante, totalD)
creDeb (Ext saldo ((_, _, Debito montante):xs)) = let (totalC,totalD) = creDeb (Ext saldo xs) 
                                                  in (totalC, totalD + montante)


--e)
saldo :: Extracto -> Float
saldo (Ext saldo []) = saldo
saldo (Ext saldo movimentos) = let (creditos,debitos) = creDeb (Ext saldo movimentos) in saldo + creditos - debitos