module CinquentaPF where
import Data.Either
--1)
myEnumFromTo::Int->Int->[Int]
myEnumFromTo x y | (x<=y) = x:myEnumFromTo (x+1) y
                 | otherwise = []


--2)
myEnumFromThenTo::Int->Int->Int->[Int]
myEnumFromThenTo x y z | (x<=z) = x:myEnumFromThenTo (x+y-1) y z
                       | otherwise = []


--3)
funcao3 :: [a] -> [a] -> [a]
funcao3 [] [] = []
funcao3 [] l = l
funcao3 l [] = l
funcao3 (x:xs) l = x:funcao3 xs l



--4)
funcao4::[a]->Int->a
funcao4 l@(x:xs) 0 = x
funcao4 l@(x:xs) i | (i>=0) = funcao4 xs (i-1)
                   | otherwise = x 


--5)
myReverse::[a]->[a]
myReverse [] = []
myReverse l@(x:xs) = (myReverse xs)++[x]



--6)
myTake::Int->[a]->[a]
myTake 0 _ = []
myTake i l@(x:xs) | (i>=0) = x:myTake (i-1) xs
                  |otherwise = []


--7)
myDrop::Int->[a]->[a]
myDrop _ [] = []
myDrop 0 l = l
myDrop i l@(x:xs) |(i>=0) = myDrop (i-1) xs
                  | otherwise = l


--8)
myZip::[a]->[b]->[(a,b)]
myZip [] [] = []
myZip l1 [] = []
myZip [] l2 = []
myZip l1@(x:xs) l2@(y:ys) = (x,y):myZip xs ys


--9)
myElem::Eq a=> a->[a]->Bool
myElem x [] = False
myElem x l@(y:ys) | (x==y) = True
                | otherwise = myElem x ys


--10)
myReplicate::Int->a->[a]
myReplicate 0 _ = []
myReplicate x c = c:myReplicate (x-1) c


--11)
intersperse::a->[a]->[a]
intersperse c (x:[]) = (x:[])
intersperse c l@(x:xs) = x:c:intersperse c xs


--12)
aux2::Eq a=>a->[a]->[a]
aux2 _ [] = []
aux2 c l@(x:xs) | (x==c) = x:aux2 c xs
                | otherwise = []

myGroup::Eq a => [a] -> [[a]]
myGroup [] = []
myGroup l@(x:xs) = (aux2 x l):myGroup (dropWhile (==x) l)


--13)
myConcat::[[a]]->[a]
myConcat [] = []
myConcat l@(x:xs) = x++myConcat xs


--14)
aux3::Int->Int->[a]->[[a]]
aux3 t _ [] = [] 
aux3 t n l | (n<=t) = (take n l):aux3 t (n+1) l
           | otherwise = []

myInits::[a]->[[a]]
myInits [] = [[]]
myInits l = aux3 (length l) 0 l

--15)
myTails::[a]->[[a]]
myTails [] = [[]]
myTails l = (take (length l) l):myTails (drop 1 l)


--16)
isPrefixOf::Eq a=>[a]->[a]->Bool
isPrefixOf [] [] = True 
isPrefixOf [] _ = True
isPrefixOf l1@(x:xs) l2@(y:ys) | (x==y) = isPrefixOf xs ys
                               | otherwise = False


--17)
isSuffixOf::Eq a=>[a]->[a]->Bool
isSuffixOf [] [] = True
isSuffixOf [] _  = True
isSuffixOf l1@(x:xs) l2@(y:ys) | (last l1 == last l2) = isSuffixOf (take (length xs) l1)  (take (length ys) l2) 
                               | otherwise = False


--18)
isSubsequenceOf::Eq a => [a]->[a]->Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf l1@(x:xs) l2@(y:ys) | (x==y) = isSubsequenceOf xs ys
                                    | otherwise = isSubsequenceOf l1 ys


--19)
aux5::Eq a=> a->Int->[a]->[Int]
aux5 c i [] = []
aux5 c i l@(x:xs) | (x==c) = i:(aux5 c (i+1) xs)
                  | otherwise = aux5 c (i+1) xs


elemIndices :: Eq a => a->[a]->[Int]
elemIndices c l@(x:xs) = aux5 c 0 l



--20)
nub::Eq a => [a]->[a]
nub [] = []
nub l@(x:xs) = x:(nub (filter (/=x) l))

--21)
delete::Eq a => a->[a]->[a]
delete _ [] = []
delete n l2@(x:xs) | (x==n) = xs
                   | otherwise = x:(delete n xs)

--22)
funcao22::Eq a=> [a]->[a]->[a]
funcao22 [] [] = []
funcao22 l1 [] = l1
funcao22 [] l1 = []
funcao22 l1@(x:xs) l2@(y:ys) = funcao22 (delete y l1) ys



--23)
contains::Eq a => a->[a]->Bool
contains n [] = False
contains n l@(x:xs) | (x==n) =True
                    | otherwise = contains n xs
                   

myUnion::Eq a =>[a]->[a]->[a]
myUnion [] [] = []
myUnion l1 [] = l1
myUnion l1@(x:xs) l2@(y:ys) | (contains y l1) = myUnion l1 ys
                            | otherwise = myUnion (y:l1) ys



--24)
myIntersect::Eq a => [a]->[a]->[a]
myIntersect [] [] = []
myIntersect [] _ = []
myIntersect l1@(x:xs) l2@(y:ys) | (contains x l2) = x:myIntersect xs l2
                                | otherwise = myIntersect xs l2


--25)
myInsert::Ord a=>a->[a]->[a]
myInsert n [] = [n]
myInsert n l@(x:xs) | (n>=x) = x:myInsert n xs
                    | otherwise = n:x:xs


--26)
myUnwords::[String]->String
myUnwords [] = []
myUnwords (x:[]) = x
myUnwords (x:xs) = x++" "++myUnwords xs


--27)
myUnlines::[String]->String
myUnlines [] = []
myUnlines l@(x:xs) = x++"\n"++myUnlines xs

--28)
aux6::Ord a=> Int->a->[a]->Int
aux6 i x [] = i
aux6 i x l@(y:ys) | (y>=x) = aux6 (i+1) y ys
                  | otherwise = aux6 i x ys 


pMaior::Ord a=>[a]->Int
pMaior [x] = 0
pMaior l@(x:xs) = aux6 0 x l


--29)
numRepetidos::Eq a=>a->[a]->Int
numRepetidos x [] = 0
numRepetidos x l@(y:ys) | (y==x) = 1 + numRepetidos x ys
                        | otherwise = numRepetidos x ys

temRepetidos::Eq a=>[a]->Bool
temRepetidos [] = False
temRepetidos l@(x:xs) | ((numRepetidos x l) /= 1) = True
                      | otherwise = temRepetidos xs


--30)
algarismos::[Char]->[Char]
algarismos [] = []
algarismos l@(x:xs) |(x>='0' && x<='9') = x:algarismos xs
                    |otherwise = algarismos xs


--31)
posImpares::[a]->[a]
posImpares [] = []
posImpares [x] = []
posImpares l@(x:y:xs) = y:posImpares xs 


--32)
posPares::[a]->[a]
posPares [] = []
posPares [x]=[x]
posPares l@(x:y:xs) = x:posPares xs


--33)
verifica::Ord a => a->[a]->Bool
verifica x [] = True
verifica x l@(y:ys) | (x<=y) = verifica y ys
                    | otherwise = False

isSorted::Ord a=>[a]->Bool
isSorted [] = True
isSorted l@(x:xs) = verifica x l 

--34)
iSort::Ord a=>[a]->[a]
iSort [] = []
iSort l@(x:xs) = myInsert x (iSort xs)


--35)
menor::String->String->Bool
menor [] [] = False
menor [] _ = True
menor l1@(x:xs) l2@(y:ys) | (x<=y) = menor xs ys
                          | otherwise = False


--36)
elemMSet::Eq a => a->[(a,Int)] -> Bool
elemMSet c [] = False
elemMSet c l@((a,i):xs) | (c==a) = True
                        | otherwise = elemMSet c xs


--37)
lengthMSet::[(a,Int)]->Int
lengthMSet [] = 0
lengthMSet l@((a,i):xs) = i + lengthMSet xs


--38)
aux7::a->Int->[a]
aux7 _ 0 = []
aux7 c x = c:aux7 c (x-1)

converteMSet::[(a,Int)]->[a]
converteMSet [] = []
converteMSet l@((a,i):xs) = (aux7 a i)++converteMSet xs


--39)
insereMSet::Eq a => a->[(a,Int)]->[(a,Int)]
insereMSet c [] = [(c,1)]
insereMSet c l@((a,i):xs) | (a==c) = (a,i+1):xs
                          | otherwise = (a,i):insereMSet c xs



--40)
removeMSet::Eq a=> a->[(a,Int)]->[(a,Int)]
removeMSet c [] = []
removeMSet c l@((a,i):xs) | (a==c) && (i==1) = xs
                          | (a==c) = (a,i-1):xs
                          | otherwise = (a,i):removeMSet c xs


--41)
aux8::Ord a =>a->[a]->Int
aux8 _ [] = 0
aux8 c (x:xs) | (x==c) = 1 + aux8 c xs
              | otherwise = aux8 c xs


constroiMSet::Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l@(x:xs) = (x, aux8 x l):constroiMSet (filter (/=x) l)


--42)
{--
partitionEithers::[Either a b] -> ([a],[b])
partitionEithers [] = ([], [])
partitionEithers l@(x:xs) = (lefts l,rigths l)
--}


--43)

catMaybes::[Maybe a] -> [a]
catMaybes [] = []
catMaybes l@(Nothing:xs) = catMaybes xs
catMaybes l@(Just x:xs) = x:catMaybes xs




--44)
data Movimento = Norte| Sul| Este| Oeste  
               deriving (Show,Eq)

data Posicao = Pos Int Int 
              deriving (Show,Eq)

posicao::(Int,Int)->[Movimento]->(Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) l@(z:zs) | (z==Norte) = posicao (x+1,y) zs
                       | (z==Sul) = posicao (x-1,y) zs
                       | (z==Este) = posicao (x,y+1) zs
                       | otherwise = posicao (x,y-1) zs


--45)
caminho::(Int,Int)->(Int,Int)->[Movimento]
caminho (x,y) (a,b) | (x<a) = Este:caminho (x+1,y) (a,b)
                    | (x>a) = Oeste:caminho (x-1,y) (a,b)
                    | (y<b) = Norte:caminho (x,y+1) (a,b)
                    | (y>b) = Sul:caminho (x,y-1) (a,b)
                    | (x==a) && (y<b) = Norte:caminho (x,y+1) (a,b)
                    | (x==a) && (y>b) = Sul:caminho (x,y-1) (a,b)
                    | (y==b) && (x<a) = Este:caminho (x+1,y) (a,b)
                    | (y==b) && (x>a) = Oeste:caminho (x-1,y) (a,b)
                    | otherwise = []



--46)
vertical::[Movimento]->Bool
vertical [] = True
vertica l@(x:xs) | (x==Norte || x==Sul) = vertical xs
                 | otherwise = False

--47)
distancia::Int->Int->Float --calcula a distancia á origem
distancia a b = (sqrt(fromIntegral(a*a) + fromIntegral(b*b)))

aux9::Int->Int->[Posicao]->Posicao
aux9 a b [] = Pos a b
aux9 a b l@((Pos x y):xs) | ( (distancia x y) < (distancia a b) ) = aux9 x y xs
                          | otherwise = aux9 a b xs

maisCentral::[Posicao]->Posicao
maisCentral l@((Pos x y):xs) = aux9 x y xs


{--48)-- ESTÁ A DAR ERROS NA QUANTIDADE DE PARAMETROS QUE ESTOU A PASSAR ÁS FUNÇÕES! tUDO LEVA A QUERER QUE ESTOU A PASSAR DE FORMA ERRADA OS PARAMETROS NO DESENVOLVER DA FUNÇÃO
verifVizinho::Int->Int->Int->Int->Bool
verifVizinho a b x y = ((x==a-1) || (x==a) || (x==a+1) && (y==b+1) || (y==b-1))


vizinhos::Posicao->[Posicao]->[Posicao]
vizinhos Pos a b [] = []
vizinhos Pos a b l@((Pos x y):xs) | (verifVizinho a b x y) = (Pos x y):vizinhos Pos a b xs
                                  | otherwise = vizinhos Pos a b xs
-}





--49)
verifOrdenada::Int->Int->[Posicao]->Bool
verifOrdenada a b [] = True
verifOrdenada a b l@((Pos x y):xs) | (y==b) = verifOrdenada a b xs
                                   | otherwise = False


mesmaOrdenada::[Posicao]->Bool
mesmaOrdenada [] = True
mesmaOrdenada l@((Pos x y):xs) = verifOrdenada x y xs


--50)
data Semaforo = Verde |Amarelo |Vermelho
              deriving (Show,Eq)


numVermelhos::[Semaforo] -> Int
numVermelhos [] = 0
numVermelhos l@(x:xs) | (x/=Vermelho) = 1 + numVermelhos xs
                      | otherwise = numVermelhos xs


interseccaoOK::[Semaforo]->Bool
interseccaoOK [] = True
interseccaoOK l | (numVermelhos l > 1) = False
                | otherwise = True