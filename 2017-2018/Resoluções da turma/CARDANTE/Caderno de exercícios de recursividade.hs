--Programação Funcional - Caderno de Exercícios 2015/2016

import Data.Char

--Exercício 1
enumFromTo'::Int->Int->[Int]
enumFromTo' x y = if (x<=y) then (x : enumFromTo' (x+1) y) else []

--Exercício 2
enumFromThenTo'::Int->Int->Int->[Int]
enumFromThenTo' x k y = if (x<=y) then (x : enumFromThenTo' (x+k) k y) else []

--Exercício 3
connect::[a]->[a]->[a]
connect [] b = b
connect (h:t) x = h : connect t x 

--Exercício 4
last'::[a]->a
last' [x] = x
last' (h:t) = last' t

--Exercício 5
init'::[a]->[a]
init' [x] = []
init' (h:t) = h : init' t

--Exercício 6
position::[a]->Int->a
position (h:t) 0 = h
position (h:t) n = position t (n-1)

--Exercício 7
reverse'::[a]->[a]
reverse' [] = []
reverse' [x] = [x]
reverse' (h:t) = reverse' t ++ [h]

--Exercício 8
take'::Int->[a]->[a]
take' n l | n<=0 = []
take' n [] = []
take' n (h:t) = h : take' (n-1) t

--Exercício 9

drop'::Int->[a]->[a]
drop' n l | n<=0 = l -- especificando o intervalo de valores de 0, elimina-se o erro da devolução imprópria da lista quando n<0
drop' n [] = [] --resolve os casos em que n > length l
drop' x (h:t) = drop' (x-1) t

--Exercício 10
zip'::[a]->[b]->[(a,b)]
zip' [] l = []
zip' l [] = []
zip' (h:t) (x:xs) = (h,x) : zip' t xs

--Exercício 11
elem':: Eq a => a->[a]->Bool
elem' n [] = False
elem' n (h:t) |(n==h) = True 
              |otherwise = elem' n t

--Exercício 12
replicate'::Int->Int->[Int]
replicate' 0 k = []
replicate' n k = k : replicate' (n-1) k

--Exercício 13
intersperse'::a->[a]->[a]
intersperse' x [] = []
intersperse' x [h] = [h] --permite que last(list) não seja sucedido por x
intersperse' x (h:t) = h : x : intersperse' x t

--Exercício 14
group'::Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h : takeWhile' (==h) t) : (group' (dropWhile' (==h) t))
--a função começa por agrupar o h com todos os elementos consecutivos iguais a h em t;
--a função é novamente invocada, mas agora aos elementos diferentes de h (dropWhile)
--realce-se para efeitos de debugging que 3:[3,3] = [3,3,3] && [1,1,1]: (cauda, daí extra []) [2:[2,2]] = [[1,1,1],[2,2,2]]

--Exercício 15
concat'::[[a]]->[a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--Exercício 16
--Criada função auxiliar para que o reverse e o acréscimo da lista inicial seja feito no fim.
--É, entao, no fim, adicionada uma lista que contém a lista l ([l])
inits'::[a]->[[a]]
inits' l = reverse (auxInits l) ++ [l]

auxInits::[a]->[[a]]
auxInits [] = []
auxInits l = (init' l : auxInits(init' l))

--Exercício 17
tails'::[a]->[[a]]
tails' [] = [[]] --necessário definir para quando tails' é aplicada a uma lista de length==2
tails' l |(length l > 1) = l : tail l : tails' (tail(tail l))
         |otherwise = [l, []]

--Exercício 18
isPrefixOf'::Eq a => [a]->[a]->Bool
isPrefixOf' l1 l2 = elem'' l1 (inits' l2) 

elem'':: Eq a => [a]->[[a]]->Bool
elem'' ls [] = False
elem'' ls (h:t) |(ls==h) = True
                |otherwise = elem'' ls t

--Exercício 19
isSuffixOf' :: Eq a =>[a]->[a]->Bool
isSuffixOf' ls l = elem'' ls (tails' l)

--Exercício 20
isSubsequenceOf':: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' l1@(h:t) (x:xs) |(h==x) = isSubsequenceOf' t xs
                                 |otherwise = isSubsequenceOf' l1 xs
{--A ordem relativa é garantida pela função, dado que ela percorre l2 até encontrar
head(l1) e, quando o fizer, caso o elemento seguinte de l1 aparecesse primeiro em l2 do que head(l1),
a função retornará falso, dado que ao percorrer l2 à procura de head(l1), a função foi eliminando as sucessivas cabeças de l2 (até encontrar h1; head(tail l1) incluído)--}


--Exercício 21
elemIndices':: Eq a => a -> [a] -> [Int]
elemIndices' a l = aux 0 a l

aux:: Eq a => Int -> a -> [a] -> [Int]
aux n a [] = []
aux n a (h:t) |(a==h) = n : aux (n+1) a t
              |otherwise = aux (n+1) a t

--Exercício 22

--Quando a == h , avança na lista, eliminando desde já a primeira ocorrência, e assim recursivamente.
--Quando a /= h, mantém esse a e aplica a função ao resto da lista, em busca de novas ocorrências de a.
deleteall::Eq a => a->[a]->[a]
deleteall a [] = []
deleteall a (h:t) |(a==h) = deleteall a t
                  |otherwise = h : deleteall a t

--Por cada h contido em t, todas as ocorrências de h (que se preserva no output) são imediatamente eliminadas de t
nub'::Eq a => [a]->[a]
nub' [] = []
nub' (h:t) |elem' h t = h : nub'(deleteall h t)
           |otherwise = h : nub' t

{-- Problema desta função nub: Retira com sucesso todas as repetições mas a lista não fica ordenada, dado que as repetições
não são eliminadas todas ao mesmo tempo, mas sim à medida que as vai encontrando.
nub''::Eq a => [a]->[a]
nub'' [] = []
nub'' list@(h:t) |(elem' h t) = nub''(delete' h list) 
--erro inicial: a função estava definida para eliminar ocorrencia de h apenas na cauda, em vez de eliminar da lista completa
                |otherwise = h : nub'' t
--}

--Exercício 23
delete':: Eq a => a->[a]->[a]
delete' x [] = []
delete' x (h:t) |(x==h) = t
                |otherwise = [h] ++ delete' x t

--Exercício 24
--Nota: Aplicou-se deleteall ao invés de delete' porque a primeira elimina apenas a primeira ocorrência; a segunda não é garantido (isto é, se l2 tiver mais do que um elem==h, l1 poderá ver eliminada mais do que uma ocorrência de h, se for o caso.
cut::Eq a => [a] -> [a] -> [a]
cut [] [] = []
cut [] x = []
cut x [] = x
cut (h:t) l2@(x:xs) |(elem' h l2) = cut t (deleteall h l2)
                    |otherwise = h : cut t l2 

--Exercício 25
union':: Eq a => [a]->[a]->[a]
union' l1 [] = l1
union' l1 (h:t) |(elem' h l1) = union' l1 t
                |otherwise = union' l1 t ++ [h] 

--Exercício 26
intersect':: Eq a => [a]->[a]->[a]
intersect' [] l2 = []
intersect' (h:t) l2 |(elem' h l2) = h : intersect' t l2
                    |otherwise = intersect' t l2

--Exercício 27
insert':: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a list@(h:t) |(a<=h) = a : list
                     |otherwise = h : insert' a t 

--Exercício 28
maximum':: Ord a => [a] -> a
maximum' [x] = x
maximum' (h:hs:t) |(h<=hs) = maximum' (hs:t)
                  |otherwise = maximum' (h:t)

--Exercício 29
minimum':: Ord a => [a] -> a
minimum' [x] = x
minimum' (h:hs:t) |(h>=hs) = minimum' (hs:t)
                  |otherwise = minimum' (h:t)

--Exercício 30
sum':: Num a => [a]->a
sum' [] = 0
sum' (h:t) = h + sum' t

--Exercício 31
product':: Num a => [a] -> a
product' [] = 1
product' (h:t) = h * product' t

--Exercício 32
and'::[Bool]->Bool
and' [] = True
and' (h:t) |(h==False) = False
           |otherwise = and' t

--Exercício 33
or'::[Bool]->Bool
or' [] = False
or' (h:t) |(h==True) = True
          |otherwise = or' t

--Exercício 34
--após integrar com caderno.hs, alterar ++ por connect (ex3)
unwords'::[String]->String
--unwords' [] = [] --caso de paragem alternativo em que string terminaria com espaço
unwords' [x] = x --caso de paragem
unwords' (h:t) = h ++ [' '] ++ unwords' t

--Exercício 35
unlines'::[String]->String
unlines' [] = [] --caso de paragem definido no prelude
--unlines' [x] = x -- caso de paragem sugerido no enunciado, onde a última string não termina com \n
unlines' (h:t) = h ++ ['\n'] ++ unlines' t

--Exercício 36
findMax::Eq a => a->[a]->Int
findMax a (h:t) |(a==h) = 0
                |otherwise = 1 + findMax a t

pMaior::Ord a => [a] -> Int
pMaior l = findMax (maximum' l) l 

--Exercício 37
temRepetidos:: Eq a => [a]->Bool
temRepetidos [] = False
temRepetidos (h:t) |(elem' h t) = True
                   |otherwise = temRepetidos t

--Exercício 38
algarismos::[Char]->[Char]
algarismos [] = []
algarismos (h:t) |((ord h >= ord '0') && (ord h <= ord '9')) = h : algarismos t
                 |otherwise = algarismos t

--Exercício 39
posImpares::[a]->[a]
posImpares [] = []
posImpares [x] = []
posImpares [x,y] = [y]
posImpares [x,y,z] = [y]
posImpares (h:x:y:z:t) = x : z : posImpares t

--Exercício 40
posPares::[a]->[a]
posPares [] = []
posPares [h] = [h]
posPares [h,x] = [h]
posPares [h,x,y] = [h,y]
posPares (h:x:y:k:t) = h : y : posPares t

--Exercício 41
isSorted:: Ord a => [a] -> Bool
isSorted [x] = True
isSorted (h:hs:t) |(h<=hs) = isSorted (hs:t)
                  |otherwise = False

--Exercício 42
iSort:: Ord a => [a]->[a]
iSort [] = [] --permite introduzir o último elemento da lista na lista de output, aplicando depois a função insert
iSort (h:t) = insert' h (iSort t)

--Exercício 43
menor::String->String->Bool
menor [] x = True
menor x [] = False
menor x y | x==y = False
menor (h:t) (x:xs) |(h<x) = True
                   |(h>x) = False
                   |otherwise = menor t xs

--Exercício 44
first::(a,b)->a
first (a,b) = a

elemMSet:: Eq a => a -> [(a, Int)] -> Bool
elemMSet a [] = False
elemMSet a (h:t) |(first h == a) = True
                 |otherwise = elemMSet a t

--Exercício 45
second::(a,b)->b
second (a,b) = b

lengthMSet::[(a,Int)]->Int
lengthMSet [] = 0
lengthMSet (h:t) = second h + lengthMSet t

--Exercício 46
replicate''::Int->a->[a]
replicate'' 0 a = []
replicate'' n a = a : replicate'' (n-1) a

converteMSet::[(a,Int)]->[a]
converteMSet [] = []
converteMSet (h:t) = replicate'' (second h) (first h) ++ converteMSet t

--Exercício 47 --Nota: Output da forma (x1,Int):[(x2,Int),(x3,Int)] == [(x1,Int),(x2,Int),(x3,Int)]!
insereMSet:: Eq a => a -> [(a,Int)]->[(a,Int)]
insereMSet a (h:t) |(first h == a) = (first h, second h + 1) : t
                   |otherwise = h : insereMSet a t

--Exercício 48
removeZeros::[(a,Int)]->[(a,Int)]
removeZeros [] = []
removeZeros (h:t) |(second h == 0) = removeZeros t
                  |otherwise = h : removeZeros t

removeMSet:: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a l@(h:t) |(elemMSet a l) && (first h==a) = removeZeros((first h, second h - 1) : t)
                     |(elemMSet a l) && (first h/=a) = removeZeros(h : removeMSet a t)
                     |otherwise = l

--Exercício 49
--Q-U-E L-I-N-D-O
takeWhile'::(a->Bool)->[a]->[a]
takeWhile' p [] = []
takeWhile' p (h:t) |p h = h : takeWhile' p t
                   |otherwise = [] --dado que é suposto interromper a 'busca' quando encontrar elem dif

dropWhile'::(a->Bool)->[a]->[a]
dropWhile' p [] = []
dropWhile' p (h:t) |p h = dropWhile' p t
                   |otherwise = (h:t) --quando deixa de verificar p, retorna a lista restante

conta::Eq a => a->[a]->Int
conta a [] = 0
conta a (h:t) |(a==h) = 1 + conta a t
              |otherwise = conta a t

constroiMSet::Ord a => [a]->[(a,Int)]
constroiMSet [] = []
constroiMSet list@(h:t) = (h, conta h list) : constroiMSet(dropWhile' (==h) t)

--Exercício 50
somaPares::[Int]->Int
somaPares [] = 0
somaPares (h:t) |(mod h 2 == 0) = h + somaPares t
                |otherwise = somaPares t

