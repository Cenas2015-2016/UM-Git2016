data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
    deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

-- |Função que introduz um novo contacto numa agenda. A inserção é feita respeitando a ordem alfabética (pelo que assume que a agenda inicial está ordenada).
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail newname newemail ag@( (name1,contacts1) : t ) 
            | (newname <= name1) = (newname, [Email newemail]) : ag
            | otherwise = (name1,contacts1) : acrescEmail newname newemail t

-- |Função que, dado um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se o nome não existir na lista, deve retornar Nothing.
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails name [] = Nothing
verEmails name ag@( (name1,contacts1) : t )
            | (name == name1) = Just k
            | otherwise = verEmails name t
            where k = filter (/= []) (map (aux) contacts1) -- recolhe emails de um contacto e coloca [] quando contacto/=email. O filter elimina as [] depois.
                  aux (Email s) = s
                  aux _ = []

-- | Dada uma lista de contactos, retorna a lista de todos os números de telefone dessa lista (tanto telefones fixos como telemóveis).
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (Casa n : t) = n : consTelefs t
consTelefs (Trab n : t) = n : consTelefs t
consTelefs (Tlm n : t) = n : consTelefs t
consTelefs (Email s : t) = consTelefs t

-- | Dado um nome e uma agenda, retorna o número de telefone de casa (caso exista).
casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa name ag@( (name1,contacts1) : t ) 
        | (name == name1) = aux contacts1
        | otherwise = casa name t  

aux :: [Contacto] -> Maybe Integer
aux [] = Nothing
aux (Trab n : t) = aux t
aux (Tlm n : t) = aux t
aux (Email n : t) = aux t
aux (Casa n : t) = Just n 


type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano
    deriving Show

type TabDN = [(Nome,Data)]

-- | Indica a data de nascimento de uma dada pessoa, caso o seu nome exista na tabela.
procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura name ( (name1,date1) : t )
    | (name == name1) = Just date1
    | otherwise = procura name t

-- | Calcula a idade de uma pessoa numa dada data.
idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade date name ( (name1,date1) : t )
    | (name == name1) = Just (aux date date1)
    | otherwise = idade date name t
    where aux (D fd fm fy) (D pd pm py) 
                | (fd >= pd) && (fm >= pm) && (fy >= py) = fy - py
                | otherwise = (fy - py) - 1

-- | Testa se uma data é anterior a outra data.
anterior :: Data -> Data -> Bool
anterior (D pd pm py) (D fd fm fy) = 
    (fy > py) || ((fy == py) && (fm > pm)) || ((fy == py) && (fm == pm) && (fd > pd)) 
 
 -- | Ordena uma tabela de datas de nascimento, por ordem crescente das datas de nascimento.
ordena :: TabDN -> TabDN
ordena [] = []
ordena (x:xs) = ins x (ordena xs)
                where ins x [] = [x]
                      ins x (b:bs) | (anterior (snd x) (snd b)) = x : b : bs
                                   | otherwise = b : (ins x bs)

-- | Apresenta o nome e a idade das pessoas, numa dada data, por ordem crescente da idade das pessoas.
porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade date l = aux date (reverse (ordena l))
                  where aux _ [] = []
                        aux date ( (name1,date1) : t ) = (name1, age date date1) : aux date t

age :: Data -> Data -> Int
age (D fd fm fy) (D pd pm py) | (fd >= pd) && (fm >= pm) && (fy >= py) = fy - py
                              | otherwise = (fy - py) - 1


data Movimento = Credito Float | Debito Float
    deriving Show

data Date = Da Int Int Int  -- Dia Mes Ano
    deriving Show

data Extracto = Ext Float [(Date, String, Movimento)]
    deriving Show

-- | Produz uma lista de todos os movimentos (cré editos ou débitos) superiores a um determinado valor.
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext n l) k = validaMovimentos k (movimentos)
                       where trd (x,y,z) = z
                             movimentos = (map (trd) l)


validaMovimentos :: Float -> [Movimento] -> [Movimento]
validaMovimentos k [] = []
validaMovimentos k ((Credito c):t)
    | c > k = (Credito c) : validaMovimentos k t
    | otherwise = validaMovimentos k t
validaMovimentos k ((Debito d):t)  
    | d > k = (Debito d) : validaMovimentos k t
    | otherwise = validaMovimentos k t

-- | Retorna informação relativa apenas aos movimentos cuja descrição esteja incluída na lista fornecida no segundo parâmetro.
filtro :: Extracto -> [String] -> [(Date,Movimento)]
filtro (Ext n l) mov = concat (aux (Ext n l) mov)
                      where aux _ [] = []
                            aux (Ext n l) (x:xs) = check l x : aux (Ext n l) xs

check :: [(Date, String, Movimento)] -> String -> [(Date,Movimento)]
check [] x = []
check ( (d1,descr1,mov1) : t ) x 
  | (descr1 == x) = (d1,mov1) : check t x
  | otherwise = check t x 

-- | Retorna o total de cré editos e de débitos de um extracto no primeiro e segundo elementos de um par, respectivamente.
creDeb :: Extracto -> (Float,Float)
creDeb (Ext n l) = (somaCreditos list,somaDebitos list)
                   where trd (x,y,z) = z
                         list = map (trd) l

somaDebitos :: [Movimento] -> Float
somaDebitos [] = 0
somaDebitos ( (Debito d) : t ) = d + somaDebitos t
somaDebitos ( (Credito d) : t) = somaDebitos t 

somaCreditos :: [Movimento] -> Float
somaCreditos [] = 0
somaCreditos ( (Credito c) : t ) = c + somaCreditos t
somaCreditos ( (Debito d) : t) = somaCreditos t

-- | Devolve o saldo final que resulta da execução de todos os movimentos no extracto sobre o saldo inicial.
saldo :: Extracto -> Float
saldo (Ext n l) = n + somaCreditos list - somaDebitos list
                  where trd (x,y,z) = z
                        list = map (trd) l

