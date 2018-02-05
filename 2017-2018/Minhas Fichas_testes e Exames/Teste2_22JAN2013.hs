module Teste2_22JAN213 where


--PARTE 1

--Exercicio 1
myMerge::Ord a => [a] -> [a] -> [a]
myMerge [] l = l
myMerge (x:xs) l = myMerge xs (insere x l)
               where
                insere:: (Ord a) => a -> [a] -> [a]
                insere n [] = [n]
                insere n (x:xs) | (n>=x) = x:insere n xs
                                | otherwise = n:x:xs



--Exercicio 2
triplos::[a]->[(a,a,a)]
triplos (x:y:[]) = []
triplos (x:y:z:xs) = (x,y,z):triplos xs

--Exercicio 3
fun::Num a => [a]->[a]
fun (x:xs) = (x*2):fun xs

--Exercicio 4
type Filme = (Titulo,Realizador,[Actor],Ano,Duracao)
type Titulo = String
type Realizador = String
type Actor = String
type Ano = Int
type Duracao = Int

filme1 = ("Tit1","Realiz1",["Act1","Act2","Act3"],1990,125)
filme2 = ("Tit2","Realiz2",["Act4","Act3","Act2"],2009,108)
filme3 = ("Tit3","Realiz3",["Act8","Act3","Act7"],2019,168)

listFilmes = [filme1,filme2,filme3]
lstTitulos = ["Tit2","Tit3","Tit1"]

doActor::Actor->[Filme]->[(Titulo,Ano)]
doActor act [] = []
doActor act f@((titulo,_,lstActores,ano,_):xs) | (existe act lstActores) = (titulo,ano):doActor act xs
                                               | otherwise = doActor act xs
                                               where
                                                  existe::Actor->[Actor]->Bool
                                                  existe a [] = False
                                                  existe a (x:xs) | (x==a) = True
                                                                  | otherwise = existe a xs

total::[Titulo]->[Filme]->Int
total [] lstFilmes = 0
total lstTitulos@(x:xs) lstFilmes@(y:ys) = (getTempo x lstFilmes) + total xs lstFilmes
                                    where
                                        getTempo::Titulo->[Filme]->Int
                                        getTempo titulo [] = 0
                                        getTempo titulo lstFilmes@((nome,_,_,_,tempo):xs) | (nome==titulo) = tempo
                                                                                          | otherwise = getTempo titulo xs


--Exercicio 5
data LTree a = Leaf a
             | Fork (LTree a) (LTree a)

leafTree = (Fork (Fork (Leaf 3) (Fork (Leaf 2) (Leaf 1))) (Fork (Leaf 4) (Fork (Leaf 12) (Fork (Leaf 10) (Leaf 5)))))


mySelect::LTree a -> [Bool] -> Maybe a
mySelect (Leaf a) [] = Just a
mySelect (Leaf a) _ = Nothing
mySelect (Fork esq dir) [] = Nothing
mySelect (Fork esq dir) (True:xs) = mySelect dir xs
mySelect (Fork esq dir) (False:xs) = mySelect esq xs


minhaRelP = [(1,3),(1,4),(2,1),(2,4),(2,5),(3,7),(4,7),(5,7),(6,5),(7,6)]

minhaRelL = [(1,[3,4]),(2,[1,4,5]),(3,[7]),(4,[7]),(5,[7]),(6,[5]),(7,[6])]

--PARTE 2
--Exercicio 1

procura::Eq a => LTree a -> a -> [[Bool]]
procura (Leaf a) = [] 
procura (Fork esq dir) = 


criaCaminho::Eq a => LTree a -> [Bool]
criaCaminho (Leaf a) = []
criaCaminho (Fork (Leaf a) dir) = False:criaCaminho dir
criaCaminho (Fork esq (Leaf a)) = True:criaCaminho esq
criaCaminho (Fork esq dir) = (criaCaminho esq) ++ (criaCaminho dir)



















--Exercicio 2

type RelP a = [(a,a)]
type RelL a = [(a,[a])]

convertePL::(Eq a) => RelP a -> RelL a
convertePL [] = []
convertePL l@((a,b):xs) = (a,criaLista a l):convertePL (filter (\(x,y)->(x/=a)) l)
                    where
                        criaLista::(Eq a) => a -> RelP a -> [a]
                        criaLista _ [] = []
                        criaLista x l@((a,b):xs) | (a==x) = b:criaLista x xs
                                                 | otherwise = criaLista x xs