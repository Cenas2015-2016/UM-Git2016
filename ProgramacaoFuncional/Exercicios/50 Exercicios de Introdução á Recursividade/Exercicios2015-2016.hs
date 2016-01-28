module PrimeiraFase where
import Data.Char

{-Exercicio 1-}
enumFromT :: Int->Int->[Int]
enumFromT x y | (x<=y) = x:enumFromT (x+1) y
              | otherwise = []



{-Exercicio 2-}
enumFromThenT :: Int->Int->Int-> [Int]
enumFromThenT x t y | (x/=y) = x:t:enumFromThenT (x+1) t y
                    | otherwise = []



{-Exercicio 3-}
(+++) :: [a]->[a]->[a]
(+++) l1 l2 = l1 ++ l2



{-Exercicio 4-}
lastV2 :: Eq a => [a] -> a
lastV2 l@(x:xs) | (xs/=[]) = lastV2 xs
                | otherwise = x


{-Exercicio 5-}
initV2 :: [a] -> [a]
initV2 (x:[]) = []
initV2 l@(x:xs) = x:initV2 xs


{-Exericio 6-}
(!!!) :: Int -> [a] -> a
(!!!) t l@(x:xs) | (t>0) = (!!!) (t-1) xs
                 | otherwise = x


{-Exercicio 7-}
reverseV2 :: [a] -> [a]
reverseV2 [] = []
reverseV2 l@(x:xs) = reverseV2 xs++[x] 


{-Exercicio 8-}
takeV2 :: Int -> [a] -> [a]
takeV2 _ [] = []
takeV2 t l@(x:xs) | (t>1) = x:takeV2 (t-1) xs
                  | otherwise = [x]


{-Exercicio 9-}
dropV2 :: Int -> [a] -> [a]
dropV2 0 l = l
dropV2 t l@(x:xs) | (t>1) = dropV2 (t-1) xs
                  | otherwise = xs



{-Exercicio 10-}
zipV2 :: [a] -> [b] -> [(a,b)]
zipV2 [] l2 = []
zipV2 l1 [] = []    
zipV2 l1@(x:xs) l2@(y:ys) = (x,y):zipV2 xs ys



{-Exercicio 11-}
elemV2 :: Eq a => a ->[a] -> Bool
elemV2 t [] = False
elemV2 t l@(x:xs) | (t==x) = True
                  | otherwise = elemV2 t xs



{-Exercicio 12-}
replicateV2 :: Int -> a -> [a]
replicateV2 0 y = []
replicateV2 x y = y:replicateV2 (x-1) y



{-Exercicio 13-}
intersperse :: a -> [a] -> [a]
intersperse t [] = []
intersperse t l@(x:xs) = x:t:(intersperse t xs)



{-Exercicio 14-} ------------------------------- REFAZER
{-group::Eq a=> [a] -> [[a]]
group [] = []
group l@(x:xs) = aux1 l ++ goup xs

aux1::Eq a=> a -> [a] ->[a]
aux1 x [] = x
aux1 (x:xs) = x:takeWhile (==x) xs ++ dropWhile ()
-}



{-Exercicio 15-}
concatV2 :: [[a]] -> [a]
concatV2 [] = [] 
concatV2 l@(x:xs) = x ++ concatV2 xs



{-Exercicio 16-} ---- COMPLETAMENTE FUNCIONAL, APENAS REVER A LOGICA
inits::[a]->[[a]]
inits [] = [[]]
inits l@(x:xs) = ((dropAuxiliar (length l) l):inits xs)

dropAuxiliar::Int->[a]->[a]
dropAuxiliar 0 l = l
dropAuxiliar _ [] = []
dropAuxiliar y l@(x:xs) = x:dropAuxiliar (y-1) xs
                        

{-Exercicio 17-}
tails::[a]->[[a]]
tails [] = []
tails l@(x:xs) = l:inits xs 


{-Exercicio 18-}
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] [] = False
isPrefixOf _ [] = False
isPrefixOf [] _ = True
isPrefixOf l1@(x:xs) l2@(y:ys) | (x==y) = isPrefixOf xs ys
                               | otherwise = False



{-Exercicio 19-}
isSufixOf::Eq a=> [a] -> [a] -> Bool
isSufixOf [] [] =False
isSufixOf [] _ = True
isSufixOf _ [] = False
isSufixOf l1@(x:xs) l2@(y:ys) | ((head$reverse$l1) == (head$reverse$l2)) = isSufixOf xs ys
                              | otherwise = False



{-Exercicio 20-}
isSubSequenceOf :: Eq a => [a] -> [a] -> Bool
isSubSequenceOf _ [] = False
isSubSequenceOf [] _ = True
isSubSequenceOf l1@(x:xs) l2@(y:ys) | (encontra x l2 == True) = isSubSequenceOf xs ys --ENCONTRADO O VALOR NA LISTA ELE TEM QUE CONTINUAR FAZER A PROCURA DO ELEMENTO SEGUINTE MAS A PARTIR DA POSIÇAO NO SEGUNDO ARRAY ONDE ELE ENCONTROU O ANTERIOR
                                    | otherwise = False 

encontra :: Eq a=> a -> [a] -> Bool
encontra t [] = False
encontra t l@(x:xs) | (t==x) = True
                   | otherwise = encontra t xs 


--------------------------------------------------------FALTA O EXERCICIO 21
{-Exercicio 22-}
{-
nub :: Eq a => [a] -> [a]
nub l@(x:xs) = aux l l2

aux:: Eq a=> [a] -> [a] -> [a]
aux [] l2 = l2
aux l1@(x:xs) l2 @(y:ys) | ((auxEncontra x l2) == False) = x++l2: aux xs l2
                         | otherwise = aux xs l2 

auxEncontra :: Eq a => a -> [a] -> Bool 
auxEncontra t [] = False
auxEncontra t l@(x:xs) | (t==x) = True
                       | otherwise = auxEncontra t xs

-}


{-Exercicio 23-}
delete :: Eq a => a -> [a] -> [a]
delete t [] = []
delete t l@(x:xs) | (t==x) = xs
                  | otherwise = x:(delete t xs)



{-Exercicio 24-}
remPrimO :: Eq a => [a] -> [a] -> [a]
remPrimO l1@(x:xs) [] = l1
remPrimO l1@(x:xs) l2@(y:ys) | (y==x) = remPrimO xs ys
                             | otherwise = x:(remPrimO xs l2)




{-Exercicio 25-}
union::Eq a=> [a] ->[a] -> [a]
union l1 [] = l1
union l1@(x:xs) l2@(y:ys) | ((pertence y l1) ==True) = union l1 ys
                          | otherwise = (union l1 ys) ++ [y]

pertence::Eq a => a-> [a] -> Bool
pertence t [] = False
pertence t l@(x:xs) | (t==x) = True
                    | otherwise = pertence t xs




{-Exercicio 26-}
intersect:: Eq a => [a] -> [a] -> [a]
intersect [] l2 = []
intersect l1@(x:xs) l2@(y:ys) | (pertence x l2==False) = intersect xs l2
                              | otherwise = x:intersect xs l2  


pertence2::Eq a => a-> [a] -> Bool
pertence2 t [] = False
pertence2 t l@(x:xs) | (t==x) = True
                    | otherwise = pertence2 t xs



{-Exercicio 27-}
insert :: Ord a => a -> [a] -> [a]
insert t [] = [t]
insert t l1@(x:xs) | (t>x) = x:insert t xs
                   | otherwise =  t:x:xs





{-Exercicio 28-}
maximumT::Ord a=>[a]->a
maximumT l@(x:xs) = maior x xs


maior::Ord a => a -> [a] -> a
maior x [] = x
maior x l@(y:ys) | (x>y) = maior x ys 
                 | otherwise = maior y ys 





{-Exercicio 29-}
minimumT::Ord a => [a] -> a
minimumT l@(x:xs) = menor x xs

menor :: Ord a => a->[a]->a
menor x [] = x
menor x l@(y:ys) | (x<y) = menor x ys 
                 | otherwise = menor y ys 





{-Exercicio 30-}
sumT::Num a=> [a]->a
sumT [] = 0
sumT l@(x:xs) = x + sumT xs






{-Exercicio 31-}
productT::Num a=> [a]->a
productT [] = 1
productT l@(x:xs) = x * productT xs






{-Exercicio 32-}
andT::[Bool]->Bool
andT [] = True
andT l@(x:xs) | (x==True) = andT xs
              | otherwise = False





{-Exercicio 33-}
orT::[Bool]->Bool
orT [] = False
orT l@(x:xs) | (x==True) = True
             | otherwise = orT xs





{-Exercicio 34-}
unwordsT::[String] ->String
unwordsT [] = []
unwordsT l@(x:xs) = x++[' ']++unwordsT xs



{-Exercicio 35-}
unlinesT::[String]->String
unlinesT [] = []
unlinesT l@(x:xs) = x++['\n']++unlinesT xs





{-Exercicio 36
pMaior::Ord a=>[a]->Int
pMaior (x:[]) = -1
pMaior l@(x:xs) | (x<(auxmaior l)) = 1 + pMaior xs
                | otherwise = pMaior xs

auxmaior::Ord a => [a] -> a
auxmaior x [] = x
auxmaior l@(x:xs) | (x>xs) =  

{-
tam::Ord a=>[a]->a
tam [] = 0
tam l@(x:xs) = 1 + tam xs

indice::Ord a=> [a] -> a -> a
indice l@(x:xs) | (x<(indice xs)) = 1 + indice xs
                | otherwise = indice xs
-}
-}






{-Exercicio 37-}
temRepetidos::Eq a=>[a]->Bool
temRepetidos [] = False
temRepetidos l@(x:xs) | (existe x xs== False) = temRepetidos xs
                      | otherwise = True

existe :: Eq a=>a->[a]->Bool
existe x [] = False
existe x l@(h:t) | (x==h) = True
                 | otherwise = existe x t





{-Exercicio 38-}
algarismos2 :: [Char]->[Char]
algarismos2 [] = []
algarismos2 l@(x:xs) | ((x >= '0') && (x<= '9')) = x:algarismos2 xs
                     | otherwise = algarismos2 xs


algarismos::[Char]->[Char]
algarismos [] = []
algarismos l@(x:xs) | (isDigit x) = x:algarismos xs
                    | otherwise = algarismos xs





{-Exercicio 39-}
posImpares :: [a]->[a]
posImpares [] = []
posImpares (x:[]) = []
posImpares (x:y:[])=[y]
posImpares l@(x:y:xs) = y: posImpares xs





{-Exercicio 40-}
posPares :: [a]->[a]
posPares [] = []
posPares (x:[]) = [x]
posPares (x:y:[])=[x]
posPares l@(x:y:xs) = x: posPares xs 





{-Exercicio 41-}
isSorted ::Ord a=>[a]->Bool
isSorted [] = True
isSorted l@(x:xs) | ((ordenado x xs)== True) = isSorted xs
                  | otherwise =  False

ordenado :: Ord a => a -> [a] -> Bool
ordenado _ [] = True
ordenado t l@(x:xs) | (t<=x) = ordenado t xs
                    | otherwise = False 




{-Exercicio 42-}
myqSort :: Ord a => [a]->[a]
myqSort [] = []
myqSort l@(x:xs) = let (menores, maiores) = divide xs x
                   in (myqSort menores) ++ [x] ++ (myqSort maiores)

divide :: Ord a => [a] -> a -> ([a],[a])
divide [] t = ([],[])
divide l@(x:xs) t = let (menores, maiores) = divide xs t
                    in if (x<t) then (x:menores,maiores)
                                else (menores,x:maiores)
{-Exericio 42/2-}
insSort:: Ord a => a -> [a]-> [a]
insSort t [] = [t]
insSort t l@(x:xs) | (t<x) = x:insSort t xs {-Apenas < e nao <= porque com <= o risco de haver mais iterações é mais alto. Nao é necessario continuar a correr o 
                                              resto da lista(dos repetidos) so para colocar um elemento igual. Assim, opta-se apenas pelo <, que assim log na 
                                              1º vez que deteta uma igualdade o elemento é logo inserido, evitando possivelmente mais algumas iterações.-}  
                   | otherwise = insSort t xs 


{-Exercicio 43-}
menorOrdLex::String->String->Bool
menorOrdLex _ [] = False
menorOrdLex [] _ = True
menorOrdLex (x:xs) (t:ts) | (x==t) = menorOrdLex xs ts
                          | otherwise = False


{-Exercicio 44-}
elemSet:: Eq a=> a->[(a,Int)]->Bool
elemSet x [] = False
elemSet x ((a,b):t) | (x==a) = True
                    | otherwise = elemSet x t


{-Exercicio 45-}
lenghtMSet :: [(a,Int)]->Int
lenghtMSet [] = 0
lenghtMSet ((a,b):xs) = b + lenghtMSet xs


{-Exercicio 46-}
convertMSet:: [(a,Int)]->[a]
convertMSet [] = []
convertMSet ((a,b):xs) | (b/=0) = a:convertMSet ((a,b-1):xs)
                       | otherwise = convertMSet xs



{-Exercicio 47-}
inserMSet ::Eq a=> a ->[(a,Int)]->[(a,Int)]
--inserMSet x [] = ((x,1):xs) --<--NÃO SEI PORQUÊ QUE NÃO FUNIONA! O ERRO ESTÁ NA MANEIRA COMO ELE CRIA UM TUPLO CASO O CARATER NAO EXISTA!
inserMSet x [] = [(x,1)] 
inserMSet x ((a,b):xs) | (x==a) = ((a,b+1):xs)
                       | otherwise = inserMSet x xs

{-Exercicio 48-}
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet t [] = []
removeMSet t ((a,0):xs) = xs
removeMSet t ((a,b):xs) | (a==t && b/=1) = (a,b-1):xs
                        | (a==t && b==1) = xs  
                        | otherwise = (a,b):removeMSet t xs

{-Exercicio 49-}
{-1º Versão-}
constroiMSet :: Ord a=> [a]->[(a,Int)]
constroiMSet [] = []
constroiMSet l1@(a:as) = (a,((contaIguais a as)+1)):constroiMSet((drop (contaIguais a as) as)) 

contaIguais :: Ord a=> a->[a]->Int 
contaIguais a [] = 0
contaIguais a l@(x:xs) | (a==x) = 1 + contaIguais a xs
                       | otherwise = contaIguais a xs

{-2ºVersão - mais logica e limpa-}
constroiMSet2 :: Ord a=> [a]->[(a,Int)]
constroiMSet2 [] = []
constroiMSet2 l@(x:xs) = (x,contaIguais2 x l):(constroiMSet2 (dropWhile (==x) l))

contaIguais2 :: Ord a=> a->[a]->Int 
contaIguais2 a [] = 0
contaIguais2 a (x:xs) | (a==x) = 1 + contaIguais2 a xs
                        | otherwise = contaIguais2 a xs

{-
--FUNÇÃO AUXILIAR do exe 49 QUE IRIA SER USADA NUMA VERSÃO MUITO BETA! :D
mSet :: Ord a=> [a] ->[(a,Int)] -> [(a,Int)]
mSet [] _ = []
mSet palavra@(a:as) set@((b,x):xs) | (b==a) = (a,x+1):mSet as xs
                                   | otherwise = (a,1):mSet as xs
-}

{-Exercicio 50-}
somaPares::[Int]->Int
somaPares [] = 0
somaPares l@(x:xs) | ((mod x 2)==0) = x + somaPares xs
                   | otherwise = somaPares xs





{-
                    FAZER:
                        21
                        36
                    REFAZER:
                        14
                    ERROS:  
                        22 - resolver o not in scope 
                        47 - Resolver prob de tipos
                    REVER:
                        16 - rever a loggica usada apenas
                        24 - 100% funcional -rever o funcionamento, perceber melhor!
-}


-----------------------------------------------------------------2º FASE --------------------------------------------------------------------
--------------------------------------------------------------ORDEM SUPERIOR-----------------------------------------------------------------
{-
MAP - Função de ordem superior que aplica uma função ao longo de uma lista
            Exemplo:
                    map :: ( a -> b ) -> [a] -> [b]
                    map f [] = []
                    map f (x:xs) = (f x) : (map f xs)

                NOTA:
                    (map f lista) é pode ser feito recorrendo a "listas de compreenção":
                            Exemplo:
                                    [f x | x <-lista]

                Aplicando:
                    minusculas s = map toLower s
                    
                        ...outro exemplo:

                    triplica t = map (*3) t


---------------------------------------------------------------------------------------------------------------------------------------------
FILTER - Função Ordem superior que filtra os lementos que verificam um dado predicado(i.e., mantem os elementos da lista para os quais o 
predicado é verdadeiro)
            Exemplo:
                    filter :: (a -> Bool) -> [a] -> [a]
                    filter p [] = []
                    filter p (x:xs) | (p x) = x:filter p xs
                                    | otherwise = filter p xs
                NOTA:
                    (filter f lista) é pode ser feito recorrendo a "listas de compreenção":
                            Exemplo:
                                    [x | x <-lista, p x]

                Aplicando:
                    primQuad ps = filter aux ps
                        where aux (x,y) = (0<x && 0<y)
                            
                            ...outro exemplo:

                    filtraDigitos s = filter isDigit s


---------------------------------------------------------------------------------------------------------------------------------------------
FUNÇÕES ANONIMAS - como ao definir estas funções não é associado um nome, estas funções dizem-se anóimas! Estas são uteis para evitar a 
declaração de funções auxiliares.
            Exemplos:
                    NO TERMINAL:                                    NO IDE:        ______
                    > (\x -> x+x) 5                                 cauda \(x:xs) -> xs  |
                    10                                              RESULTADO: xs        |
                                                                                         |------> $cauda [1,2,3,4]
                    ...outro exemplo:                               cauda \(_:xs) -> xs  |        [2,3,4]
                                                                    RESULTADO: xs  ______|
                    > (\x -> x:x-1:x-2:x-3:[]) 3
                    > [3,2,1,0]                                     trocaPares xs = map(\(x,y) -> (y,x)) xs


---------------------------------------------------------------------------------------------------------------------------------------------
FOLDR - Associação á direita, note: foldR
            Exemplos:
                    sum xs = foldr (+) 1 xs
                    and bs = foldr (&&) True bs
                    concant ls = foldr (++) ls

                Exemplos ao pormenor:
                        (product [1,2,3]) => 1*(2*(3*1)) => 60
                            
                            ...outro exemplo:
                        
                        (concat [[3,4,5],[2,1],[7,8]]) => [3,4,5] ++ ([2,1] ++ ([7,8] ++ [])) => [3,4,5,2,1,7,8]

FOLDL - Associação á esquerda, note: foldL
------------------------------------------------------RESUMO DAS DIFERENÇAS ENTRE foldr e foldl----------------------------------------------               
            Diferenças entre FOLDR e FOLDL:
                                            foldr (-) 8 [4,7,3,5] => 4-(7-(3-(5-8))) => 3
                                            foldl (-) 8 [4,7,3,5] => (((8-4)-7)-3)-5 => -11
---------------------------------------------------------------------------------------------------------------------------------------------



-}



---------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------EXERCICIOS EXISTENTES NA FICHA 4 DE 2015-2016----------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------
{-Exercicio 3-}
type Mset a = [(a,Int)]
--a)
{-
1º versão (não está completamente correta)
union2:: Eq a=> Mset a -> Mset a -> Mset a --confirmar se funcina correto
union2 [] _ = []
union2 l1@((x,y):xs) l2@((c,d):ts) = (obtemTuplo (x,y) l2):(union2 xs l2)
                                   
obtemTuplo :: Eq a=> (a,Int) -> Mset a -> (a,Int)
obtemTuplo (x,y) [] = (x,y)
obtemTuplo (x,y) l2@((c,d):ts) | (x==c) = (x,y+d)
                               | otherwise = obtemTuplo (x,y) ts
-}
{-2º versão-}
union' :: Eq a => [(a,Int)] -> [(a, Int)] -> [(a, Int)]
union' l l2 =  constroiMSet' (converteMSet' (l++l2)) 

constroiMSet'::Eq a => [a]->[(a,Int)]
constroiMSet' [] = []
constroiMSet' list@(h:t) = (h, conta' h list) : constroiMSet'(deleteall h list)

conta'::Eq a => a->[a]->Int
conta' a [] = 0
conta' a (h:t) |(a==h) = 1 + conta' a t
              |otherwise = conta' a t

converteMSet'::[(a,Int)]->[a]
converteMSet' [] = []
converteMSet' (h:t) = replicate (snd h) (fst h) ++ converteMSet' t

deleteall::Eq a => a->[a]->[a]
deleteall a [] = []
deleteall a (h:t) |(a==h) = deleteall a t
                  |otherwise = h : deleteall a t

--b)
intersect3 :: Eq a=> Mset a -> Mset a -> Mset a
intersect3 [] _ = []
intersect3 l1@((a,b):xs) l2@((c,d):ts) | ((existe3 a l2) == True) = (a,if (b<=d) then b else d):intersect3 xs l2
                                       | otherwise = intersect3 xs l2

existe3 :: Eq a => a -> Mset a -> Bool
existe3 t [] = False
existe3 t ((c,d):ts) | (t==c) = True
                     | otherwise = existe3 t ts


--c)
diff:: Eq a=> Mset a -> Mset a -> Mset a
diff [] [] = []
diff l1 [] = l1
diff [] l2 = []
diff l1@((a,b):xs) l2@((c,d):ts) = aux((devolveTuplo (a,b) l2):diff xs l2)


devolveTuplo :: Eq a => (a,Int) -> Mset a -> (a,Int)
devolveTuplo (a,b) [] = (a,b)
devolveTuplo (a,b) l2@((c,d):ts) | (a==c) = (a,if ((b-d)<0) then ((b-d)*(-1)) else (b-d))
                                 | otherwise = devolveTuplo (a,b) ts

aux :: [(a,Int)]->[(a,Int)] --condição para nao adicionar tuplos com inteiros = o
aux [] = []
aux (h:t) | (snd h == 0) = aux t
          | otherwise = h : aux t


--d)
ordena :: Mset a -> Mset a
ordena [] = []
ordena l@((a,b):xs) = let (menores, maiores) = ordenador (a,b) xs
                      in (ordena menores) ++ [(a,b)] ++ (ordena maiores)

ordenador :: (a,Int) -> Mset a -> (Mset a, Mset a)
ordenador (a,b) [] = ([],[])
ordenador (a,b) ((c,d):xs) = let (menores, maiores) = ordenador (a,b) xs 
                             in if (d<=b) then ((c,d):menores,maiores)
                                          else (menores, (c,d):maiores)                        



--e)
--Começa por colocar o elemento com maior contagem após ordenada a lista de Mset's e depois aplica a função aux para colocar elementos
-- com igual contagem à do primeiro.
moda :: Mset a -> [a]
moda l = fst(head (reverse (ordena l))) : aux' ((reverse(ordena l)))

--Função que cria uma lista com os primeiros elementos de cada par com contagem igual à do maior.
aux' :: Mset a -> [a]
aux' [] = []
aux' [x] = []
aux' (h:hs:t) |(snd hs == snd h) = (fst hs) : aux' (h:t)
              |otherwise = aux' (h:t)





---------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------EXERCICIOS EXISTENTES NA FICHA 5 DE 2015-2016----------------------------------------------
----------------------------------------------------------EXERCICIOS DE ORDEM SUPERIOR-------------------------------------------------------
type Polinomio  = [Monomio]
type Monomio = (Float, Int)

--a)
conta'' :: Int -> Polinomio -> Int
conta'' t [] = 0
conta'' t ((x,y):xs) | (y==t) = 1 + conta'' t xs
                     | otherwise = conta'' t xs

--b)
grau'' :: Polinomio -> Int
grau'' [] = 0
grau'' ((x,y):xs) = maior' y xs
                  
maior'::Int -> Polinomio ->Int
maior' t [] = t
maior' t ((x,y):xs) | (t<y) = maior' y xs
                    | otherwise = maior' t xs


--c)
{-
1º Versão(abaixo):
Perceber porquê que não funciona?!; Filter está mal aplicado?

selgrau t l@((x,y):xs) = filter monomioIguala l
                        where monomioIguala ((x,y):xs) | (t==y) = (x,y):selgrau t xs
                                                       | otherwise = selgrau t xs

      {-
      Baseado no exemplo:
                      primQuad ps = filter aux ps
                              where aux (x,y) = (0<x && 0<y)
      -}    
-}

{-2º Versão(abaixo) - FUNCIONAL!-}
selgrau :: Int -> Polinomio -> Polinomio
selgrau t [] = []
selgrau t l@((x,y):xs) = filter (\(x,y)->(y==t)) l
      {-
      Baseado no exemplo:
          Input: filter (\x -> length x > 4) ["aaaa","bbbbbbbbbbbbb","cc"]
          Output: ["bbbbbbbbbbbbb"]
      -}


--d)
{-1º Versão-}
deriv::Polinomio->Polinomio
deriv [] = []
deriv p@((x,y):xs) = map (\(x,y)->(if (y>1) then (x,y-1) else (x,0))) p



{-2ºVersão - Usando uma função como condicional e passando para a mesma o input a ser avaliado,i.e (x,y).
A função não faz completamente o que é pedido pois nao consegui resolver o problema entre os tipos, i. e, multiplicar float com inteiro.-}
deriv2::Polinomio->Polinomio
deriv2 [] = []
deriv2 p@((x,y):xs) = map (\(x,y)->condicao (x,y)) p


condicao::Monomio->Monomio
condicao (x,y) | (y>1) = (x,y-1)
               | otherwise = (x,0)

--e)
calcula ::Float -> Polinomio -> Float
calcula t [] = 0
calcula t l@((x,y):xs) = ((t*x)^y) + calcula t xs
                       

--f)
simp::Polinomio->Polinomio
simp l@((x,y):xs) = filter (\(x,y) -> (x/=0)) l 


--g)
mult::Monomio->Polinomio->Polinomio
mult (x,y) l@((a,b):xs) = map (\(a,b) -> (x*a,y+b)) l






normaliza ::Polinomio->Polinomio
normaliza [] = []
normaliza l1@((x,y):t) = compacta l1 l2

compacta ::Polinomio->Polinomio->Polinomio
compacta [] l2 = l2
compacta l1@((x,y):t) l2@((a,b):xs) = (existeParaCompact (x,y) l2):compacta t l2 

existeParaCompact::Monomio->Polinomio->Monomio
existeParaCompact (x,y) [] = (x,y)
existeParaCompact (x,y) l@((a,b):xs) | (y==b) = existeParaCompact (x+a,y) xs
                                     | otherwise = existeParaCompact (x,y) xs


--h)
{-1º Versão-}
{-normaliza :: Polinomio -> Polinomio
normailza l@((x,y):xs) = map recriaTuplos l
                         where  recriaTuplos [] = []
                                recriaTuplos ((a,b):bs) | (b==y) = (x+a,y):recriaTuplos bs 
                                                        | otherwise = (x,y)
-}
{-2ºVersão - tambem nao consigo por a funcionar, supostamente a parte do codigo a baixo que está comentada devia de funcionar, mas nao se confirma-}
--normaliza2::Polinomio->Polinomio
--normaliza2 [] = []
--normaliza2 l@((x,y):xs) =  

--agrupaMonomios:: Polinomio->Polinomio
--agrupaMonomios l@((x,y):xs) = aux ((filter (\(a,b)->(b==y)) l)++(filter (\(a,b)->(b/=y)) l) )
--              where aux [] = []
--                    aux (h:t) = ((foldr1 (+) (map (fst) h)),(snd(head h))) : aux t


--take 1$(map (\(x,y) -> (somaTuplos (x,y) xs)) l)++normaliza2 xs 
--map (\(x,y) -> (take 1$(map (\(x,y) -> (somaTuplos (x,y) xs)) l))) l  
--(take 1$(map (\(t,s)->(x+t,y)) (filter (\(a,b) -> (b==y)) l))) -- :normaliza2 xs

--somaMonomios::(Float,Int)->Polinomio->Monomio
--somaMonomios (a,b) [] = (a,b)
--somaMonomios (a,b) l@((x,y):xs) | (y==b) = somaMonomios (a+x,b) xs
--                                | otherwise = somaMonomios (a,b) xs


--somaPolinomios :: Polinomio->Polinomio->Polinomio
--somaPolinomios l1@((x,y):xs) l2@((z,t):zs) =  map (\(a,b) -> (if (t==y) then (x+z,y) else (x,y))) l1

--soma :: (Float,Int) -> Polinomio -> Float
--soma (t,p) [] = t
--soma (t,p) l@((x,y):xs) | (p==y) = (t+x,p):soma (t,p) xs
 --                       | otherwise = soma (t,p) xs 



--j)
--produtoPolinomios :: Polinomio -> Polinomio -> Polinomio
--produtoPolinomios l1@((x,y):xs) l2@((a,b):as) = map (\(x,y) -> ) l1 l2
--where aux2 (x,y) l2@((a,b):as) | (b==y) = (x*a,y+b)
--                               | otherwise = aux2 (x,y) as




---------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------EXAME DE 2015 - 9 de Fevereiro 2015---------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------
type TurmaL = [(Numero,Aluno)]
type Aluno = (Nome,Nota)
type Numero = Int
type Nome = String
type Nota = Float

--a)
taxaAp::TurmaL->Float 
taxaAp [] = 0
taxaAp l =  ((admitidos l)/fromIntegral(length l))

admitidos::TurmaL->Float
admitidos ((number,(name,note)):xs) | (note>9.5) = 1 + taxaAp xs
                                    | otherwise = taxaAp xs

--b)
--  map (\(z,(x,y))->x) -----> captura apenas os nome dos alunos contidos na lista: (take n (ordenaNotas l))
-- (take n (ordenaNotas l)) ---> Apresenta a lista apenas com o numero de alunos definido pelo utilizador, mas ainda com os dados todos. 
top::Int->TurmaL-> [String]
top _ [] = error "A turma nao contem alunos!"
top 0 _ = error "Numero de alunos inserido invalido!"
top n l@((z,(x,y)):t) = map (\(z,(x,y))->x) (take n (ordenaNotas l))


--Ordena a lista de alunos por ordem de melhores notas para as mais baixas, manipulando dados completos de cada aluno.
ordenaNotas::TurmaL->TurmaL
ordenaNotas [] = []
ordenaNotas ((z,(x,y)):t) = let (baixas, altas) = part t (z,(x,y))
                            in (ordenaNotas altas) ++ [(z,(x,y))] ++ (ordenaNotas baixas)

part :: TurmaL -> (Int,(String,Float)) -> (TurmaL,TurmaL)
part [] (num,(nome,nota)) = ([],[])
part ((z,(x,y)):t) (num,(nome,nota)) = let (menores,maiores) = part t (num,(nome,nota))
                                       in if (y<=nota) then ((num,(x,y)):menores,maiores)
                                                       else (menores,(num,(x,y)):maiores)


--c)
--uma turma calcula o comprimento do nome mais longo.
lNomeMax :: TurmaL -> Int
lNomeMax [] = 0
lNomeMax l = maximum (ordenaTamanhos(listaTamanhos l))

--uma turma calcula o comprimento do nome mais longo. (nesta versão nao tenho necessidade de fazer uma ordenação da turma, logo mais compacta e rápida!)
lNomeMax2 :: TurmaL -> Int
lNomeMax2 [] = 0
lNomeMax2 l = maximum (listaTamanhos l)

--cria uma lista dos tamanhos dos nomes dos alunos
listaTamanhos :: TurmaL -> [Int]
listaTamanhos l@((z,(x,y)):t) = map (\(z,(x,y)) -> (length x)) l
--ordena a lista dos tamanhos
ordenaTamanhos :: [Int]->[Int]
ordenaTamanhos [] = []
ordenaTamanhos l@(h:t) = let (menores,maiores) = separa t h
                         in (ordenaTamanhos maiores) ++ [h] ++ (ordenaTamanhos menores)
separa :: [Int] -> Int -> ([Int],[Int])
separa [] x = ([],[])
separa (h:t) x = let (menores,maiores) = separa t x
                 in if (h<x) then (h:menores,maiores)
                             else (menores,h:maiores)

--d)
listaT::TurmaL -> IO () 
listaT [] = error "Não existe nenhuma turma!"
listaT l@((a,(x,y)):xs) = putStrLn (unlines(map (linha) l))


--consoante a condição "linha" cria apenas uma linha de um aluno
linha :: (Numero,Aluno) -> String
linha (a,(x,y)) | (y>=9.5) = show a ++ " " ++ x ++ " " ++ show y
                | otherwise = show a ++ " " ++ x ++ " " ++ "R"

----------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------Alguns exercicios a respeito de Arvores Binárias para seguimento dos exercicios deste exame---------------------------------------
data ArvBin a = Vazia
              | Nodo a (ArvBin a) (ArvBin a)

altura :: ArvBin a -> Integer
altura Vazia = 0
altura (Nodo _ e d) = 1 + max (altura e) (altura d)