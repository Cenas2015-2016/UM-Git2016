{-EXERCICIOS EXTRA DE ORDENAÇÃO DE VETORES/ARRAYS-}
---QuickSort---TUDO FUNCIONAL

--Ordenar uma lista por ordem crescente, é possivel usar em MSets
quicksort::Ord a=>[a]->[a]
quicksort [] =[]
quicksort (x:xs) = let (menores, maiores) = part xs x
                   in (quicksort menores) ++ [x] ++ (quicksort maiores)

part::Ord a=>[a]->a->([a],[a])
part [] x = ([],[])
part (h:t) x = let (men,mai)=part t x
               in if (h<x) then (h:men,mai)
                           else (men,h:mai) 

----------------------------------------------------------------------------
--Inserir um elemento numa lista ordenada
insertSort :: Ord a=> a->[a]->[a]
insertSort t [] = [t]
insertSort t l@(x:xs) | (t<=x) = t:l
                      | otherwise = x:insertSort t xs


----------------------------------------------------------------------------
--QuickSort refeito, apenas com o intuito de ser mais explicita a sua compreensão
qkSort::Ord a=> [a]->[a]
qkSort [] = []
qkSort l@(x:xs) = let (menores, maiores) =  divide xs x
                  in  (qkSort menores) ++ [x] ++ (qkSort maiores)
{-
no output da seguinte função:
lista = cauda da lista do input
-elemento singular é o pivot usado para fazer as comparações
-o tuplo de listas diz respeito ao tupplo composto pela lista dos menores e pela lista dos maiores, tuplo esse que será concatenado juntamente com o pivot na função qkSort.
-}
divide :: Ord a=> [a] -> a -> ([a],[a])
divide [] pivot = ([],[])
{-
no let:
para as listas "menores" e "maiores" é aplicada a função "divide". 
A função "divide" consiste na aplicação da condição (c<=pivot) que distribui os resultados pelas listas ""declaradas"" no "let", isto é, "menores" e "maiores". 
Repetindo o procedimento tantas vezes até que a lista de input seja lista vazia.
-}
divide lista@(c:cs) pivot =  let (menores, maiores) = divide cs pivot
                             in if (c<=pivot) then (c:menores, maiores)
                                              else (menores,c:maiores)

---------------------------------------------------------------------------------
--já com recurso a ordem superior
qSort1 :: Ord a => [a] -> [a]
qSort1 [] = []
qSort1 l@(h:t) = qSort1 menores ++ [h] ++ qSort1 maiores
                 where menores = filter (<=h) t
                       maiores = filter (>h) t



---------------------------------------BEST OF THEM------------------------------
--O quickSort mais compacto e facil de entender! Não é possivel usar em MSets
--TIPO DE DADOS QUE TRATA:  [Int]
qSort2 :: Ord a => [a] -> [a]
qSort2 [] = []
qSort2 l@(h:t) = qSort2 (filter (<=h) t) ++ [h] ++ qSort2 (filter (>h) t)


--Ordenação de MSets com recurso apenas a ordem superior
--TIPO DE DADOS QUE TRATA:  [(Float,Int)]
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena l@((x,y):xs) = (ordena (filter(\(a,b)->(b<=y)) xs)) ++ [(x,y)] ++ (ordena (filter(\(a,b)->(b>y)) xs))


--ISORT - Ordena MSets 
--TIPO DE DADOS QUE TRATA:  [(Float,Int)]
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (x:xs) = ins x (ordena xs)
              where ins x [] = [x]
                    ins x (b:bs) |(snd x <= snd b) = x : b : bs
                                 |otherwise = b : (ins x bs)

