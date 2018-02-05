module Exame17FEV2012 where



--------------------------------PARTE 1------------------------------
--Exercicio 1
posNeg::[Int] -> (Int,Int)
posNeg l = aux l (0,0)
        where
            aux:: [Int] -> (Int,Int) -> (Int,Int)
            aux [] (a,b) = (a,b)
            aux l@(x:xs) (a,b) | (x>=0) = aux xs (a+1,b)
                               | otherwise = aux xs (a,b+1)



--Exercicio 2
tiraPrefixo::String->String->Maybe String
tiraPrefixo _ [] = Nothing
tiraPrefixo l1 l2 | ((length l1)==(aux l1 l2 0)) = Just (drop (aux l1 l2 0) l2)
                  | otherwise = Nothing
                  where
                    aux::String -> String -> Int -> Int
                    aux _ [] n = n
                    aux [] _ n = n
                    aux (x:xs) (y:ys) n | (x==y) = aux xs ys (n+1)
                                        | otherwise = n


--Exercicio 3
{--
fun:: (a->b)->[Int]->[Int]
fun f l@(x:xs) = product (f (take 1 (aux pred l)):fun f xs)
                where
                    aux::(a->b)->[Int] -> [Int]
                    aux f [] = []
                    aux f (x:xs) | (f x) == x:aux f xs
                                 | otherwise = aux f xs

-}
--Exercicio 4
data ArvBin a = Vazia
              | Nodo a (ArvBin a) (ArvBin a)

insere::Ord a => a -> ArvBin a -> ArvBin a
insere a Vazia = (Nodo a Vazia Vazia)
insere x (Nodo a esq dir) | (a<=x) = (Nodo a esq (insere x dir))
                          | otherwise = (Nodo a (insere x esq) dir)


--Exercicio 5
type Concorrentes = [(Numero,String)]
type Numero = Integer
type Prova = [(Numero,Float)]


lstConcorrentes = [(5,"Jorge"),(3,"Xavier"),(6,"Ana"),(1,"Sofia"),(2,"Rodrigo")]
prova = [(1,34.5),(2,34.0),(5,40.0),(3,34.0),(6,25.0)]






nomes::Prova->Concorrentes->[(String,Float)]
nomes [] _ = []
nomes l1@(x:xs) l2 = (aux x l2):nomes xs l2
                    where
                        aux::(Numero,Float)->Concorrentes->(String,Float)
                        aux (num,tempo) l@((n,nome):xs) | (n==num) = (nome,tempo)
                                                        | otherwise = aux (num,tempo) xs







ordena::Prova->Prova
ordena [] = []
ordena [x] = [x]
ordena l@(x:xs) = myIsort x (ordena xs)
                where
                    myIsort::(Numero,Float)->Prova->Prova
                    myIsort (a,b) [] = [(a,b)]
                    myIsort (a,b) l@(x:xs) | (b>=(snd x)) = x:myIsort (a,b) xs
                                           | otherwise = (a,b):l


--------------------------------PARTE 2---------------------------------
--Exercicio 1

data ArvBin2 a = Empty
               | Node a (ArvBin2 a) (ArvBin2 a)

arvore2 = (Node 10 (Node 5 Empty (Node 6 Empty Empty)) (Node 15 (Node 13 (Node 12 Empty Empty) (Node 14 Empty Empty)) (Node  16 Empty Empty)))
{--
                     10
                     /\
                    /  \
                   /    \
                  /      \
                 /        \
                /          \
               /            \
              /              \
             5                15
              \              /\
               \            /  \
                6          /    \
                         13      16
                         /\
                        /  \
                       /    \
                      12    14

-}

lstBoolValues1 = [True,False]
lstBoolValues2 = [True,False,True,True]
lstBoolValues3 = [True,False,True]


camValido::ArvBin2 a -> [Bool] -> Bool
camValido Empty _ = False
camValido _ [] = True
camValido (Node a esq dir) (True:xs) = camValido dir xs
camValido (Node a esq dir) (False:xs) = camValido esq xs




caminho::(Eq a,Ord a) => ArvBin2 a -> a -> Maybe [Bool]
caminho Empty a = Nothing
caminho arvore valor | (existe arvore valor) = Just (constroiCam arvore valor)
                     | otherwise = Nothing

existe::(Eq a,Ord a) => ArvBin2 a -> a -> Bool
existe Empty n = False
existe (Node a esq dir) n | (n==a) = True
                          | otherwise = (existe esq n) || (existe dir n)


constroiCam::(Eq a,Ord a) => ArvBin2 a -> a -> [Bool]
constroiCam Empty n = []
constroiCam (Node a esq dir) n | (n>a) = True:constroiCam dir n
                               | (n<a) = False:constroiCam esq n
                               | otherwise = []




