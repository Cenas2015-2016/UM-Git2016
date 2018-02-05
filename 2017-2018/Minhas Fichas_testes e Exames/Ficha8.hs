module Ficha8 where
import Data.Char
--Exercicio 1 
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

teste = (Mais (Const 3) (Menos (Const 2) (Const 5)))
--(O resultado do teste a cima tem que dar 0)
{--
               (+) <------------- 3 + (2 - 5)    
               / \
              /   \
             /     \
            3      (-) <--------- 3 +
                   /\
                  /  \
                 2    5 <-------- 2 - 5

OBS: Mais observações sobre Arvores de Expressão no livro PF do JBB pagina 147
-}


--a)
calcula::ExpInt->Int
calcula (Const x) = x
calcula (Simetrico x) = (-1)*(calcula x)
calcula (Mais x y) = (calcula x) + (calcula y)
calcula (Menos x y) = (calcula x) - (calcula y)
calcula (Mult x y) = (calcula x) * (calcula y)


--b)
infixa::ExpInt->String
infixa (Const x) = [intToDigit x]
infixa (Simetrico x) = " - " ++ (infixa x)
infixa (Menos x y) = "("++ (infixa x) ++ " - " ++ (infixa y) ++ ")"
infixa (Mais x y) = "("++ (infixa x) ++ " + " ++ (infixa y) ++ ")"
infixa (Mult x y) = "("++ (infixa x) ++ " x " ++ (infixa y) ++ ")"


--c)
posfixa::ExpInt->String
posfixa (Const x) = [intToDigit x] ++ " "
posfixa (Simetrico x) = "-" ++ (posfixa x)
posfixa (Menos x y) = (posfixa x) ++ (posfixa y) ++ " -"
posfixa (Mais x y) = (posfixa x) ++ (posfixa y) ++ " +"
posfixa (Mult x y) = (posfixa x) ++ (posfixa y) ++ " x"



--Exercicio 2
data RTree a = R a [RTree a]
        deriving Show
rt1 = R 10 [R 4 [R 7 [R 1 [], R 2 []]],R 8 [R 9 [], R 30 []]] 
{--
teste2 = (R 2 [(R 25
                    [(R 43 [(R 100 []),
                            (R 30 [])]),
                     (R 2 []),
                     (R 4 [])],
                 (R 3 []),
                 (R 4 [(R 100 [(R 20 []),
                               (R 3 []),
                               (R 2 []),
                               (R 4 [])])]))])
-}
--a)

soma::Num a => RTree a->a
soma (R a []) = a
soma (R a (x:xs)) = a + soma x + soma (aux xs)
                 where  aux :: Num a => [RTree a] -> RTree a 
                        aux [] = R 0 []
                        aux [rTree] = rTree 



--b)
altura :: RTree a ->Int
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l)

{--
Quase funcional mas conta sempre 1 a menos na altura
altura :: RTree a ->Int
altura (R a rts@(x:xs)) = 1 + (maximum (map (rTreeLength) rts))
                where rTreeLength::RTree a -> Int
                      rTreeLength (R a []) = 1
                      rTreeLength (R a rts@(x:xs)) = 1 + length rts
-}
--c)

prune::Int->RTree a ->RTree a
prune 1 (R a l) = R a []
prune x (R a l) = (R a (map (prune (x-1)) l))



mirror::RTree a -> RTree a
mirror (R a []) = (R a [])
mirror (R a l) = R a (map mirror (reverse l))


postOrder::RTree a -> [a]
postOrder (R a []) = [a]
postOrder (R a l) = foldr (++)  [a] (map postOrder l)



preOrder::RTree a -> [a]
preOrder (R a []) = [a]
preOrder (R a l) = foldl (++)  [a] (map preOrder l)

---------------------------------Extras-------------------------------
{--
Conta o numero de nodos da RTree
--->Para uma arvore vazia o numero de nodos é zero
--->o numero de nodos de uma arvore da forma R r [a1..an] nao é 
    mais do que 1 mais o somatorio do numero de nodos de todas as 
    sub-arvores a1..an
-}
contaNodos::RTree a ->Int
--contaNodos Empty = 0
contaNodos (R a []) = 1
contaNodos (R a rtrs@(x:xs)) = 1 + (sum(map contaNodos rtrs))
{--
Calcula o peso de uma arvore
--->O peso de uma arvore vazia é zero
--->o peso de uma arvore da forma R r [a1..an] nao é mais do que 
    1 mais o maior dos pesos de todas as sub-arvores a1..an
-}
peso::RTree a ->Int
--peso Vazia = 0
peso (R a []) = 1
peso (R a rtrs@(x:xs)) = 1 + (maximum(map peso rtrs))



--Exercicio 3
data BTree a = Empty 
             | Node a (BTree a) (BTree a)

data LTree a = Tip a 
             | Fork (LTree a) (LTree a)
leafTreeTeste = (Fork (Fork (Fork (Tip 4) (Tip 10)) (Tip 3)) (Tip 1))
{--
                      FORK <-----------ALTURA = 0
                       /\
                      /  \
                     /    \
                    /      \
                  FORK      1  <-------ALTURA = 1
                   /\
                  /  \
                 /    \
               FORK    3  <-----------ALTURA = 2
                /\
               /  \
              /    \
             4     10  <--------------ALTURA = 3
-}
--a)
ltSum::Num a => LTree a -> a
ltSum (Tip a) = a
ltSum (Fork left right) = ltSum left + ltSum right



--b)
listaLT :: LTree a -> [a]
listaLT (Tip a) = [a]
listaLT (Fork left right) =  (listaLT left) ++ (listaLT right) --PRE-ORDER
--listaLT (Fork left right) =  (listaLT right) ++ (listaLT left) --POS-ORDER


--c)
ltHeight :: LTree a -> Int
ltHeight (Tip a) = 0
ltHeight (Fork left right) = 1 + (max (ltHeight left) (ltHeight right))


--Exercicio 4)
data FTree a b = Leaf b 
               | No a (FTree a b) (FTree a b)
{--
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree ft@(FTree a b) = let (x,y) in aux ft (x,y)
                         where aux (Leaf b) = [x] ++ aux b
                               aux (FTree a b)

--}

{--
OBS:
-rever a função altura de uma RTree (Exe 2 b))
-preceber como se faz a função prune (Exe 2 c))
-Não compreendo o nada do exercicio 4

-}