module CinquentaAntesDoTeste where
import Data.Either
--1)
myEnumFromTo::Int->Int->[Int]
myEnumFromTo a b | (a<=b) = a:myEnumFromTo (a+1) b
                 | otherwise = []

--2)
myEnumFromThenTo::Int->Int->Int->[Int]
myEnumFromThenTo a b c |(a<=c) = a:myEnumFromThenTo (a+b-1) b c
                       | otherwise = []

--3)
concatena::[a]->[a]->[a]
concatena [] [] = []
concatena [] l = l
concatena l [] = l
concatena (x:xs) l@(y:ys) = x:concatena xs l


--4)
funcao4::[a]->Int->a
funcao4 l@(x:xs) 0 = x
funcao4 l@(x:xs) i | (i>=0) = funcao4 xs (i-1) 
                   | otherwise = x

--5)
myReverse::[a]->[a]
myReverse [] = []
myReverse l@(x:xs) = (myReverse xs) ++ [x]

--6)
myTake::Int->[a]->[a]
myTake n l@([]) = l
myTake n l@(x:xs) | (n>0) = x:myTake (n-1) xs
                  | otherwise = []



--7)
myDrop::Int->[a]->[a]
myDrop n [] = []
myDrop 0 l = l
myDrop n l@(x:xs) = myDrop (n-1) xs



--8)
myZip::[a]->[b]->[(a,b)]
myZip [] [] = []
myZip l [] = []
myZip [] l = []
myZip l1@(x:xs) l2@(y:ys) = (x,y):myZip xs ys


--9)
myElem::Eq a=>a->[a]->Bool
myElem _ [] = False
myElem c l@(x:xs) | (x==c) = True
                  | otherwise = myElem c xs


--10)
myReplicate::Int->a->[a]
myReplicate 0 _ = []
myReplicate n c = c:myReplicate (n-1) c


--11)
myIntersperse::a->[a]->[a]
myIntersperse c [] = []
myIntersperse c l@(x:[]) = l 
myIntersperse c l@(x:xs) = x:c:myIntersperse c xs


--12)
aux::Eq a=> a->[a]->[a]
aux _ [] = []
aux c l@(x:xs) | (x==c) = x:aux c xs
               | otherwise = []


myGroup::Eq a=>[a]->[[a]]
myGroup [] = []
myGroup l@(x:xs) = (aux x l):myGroup (dropWhile (==x) l)



--13)
myConcat::[[a]]->[a]
myConcat [] = []
myConcat l@(x:xs) = x ++ myConcat xs


--14)
aux2::Int->Int->[a]->[[a]]
aux2 i n l@(x:xs) | (i<=n) = (take i l):aux2 (i+1) n l
                  | otherwise = []


myInits::[a]->[[a]]
myInits [] = []
myInits l@(x:xs) = aux2 0 (length l) l

--15)
myTails::[a]->[[a]]
myTails [] = []
myTails l@(x:xs) = l:myTails (drop 1 l)


--16)
myIsPrefixOf::Eq a=>[a]->[a]->Bool
myIsPrefixOf [] [] = True
myIsPrefixOf [] _ = True
myIsPrefixOf l1@(x:xs) l2@(y:ys) | (x==y) = myIsPrefixOf xs ys
                                 | otherwise = False


--17)
myIsSufixOf::Eq a=>[a]->[a]->Bool
myIsSufixOf [] [] = True
myIsSufixOf [] _ = True
myIsSufixOf _ [] = False
myIsSufixOf l1 l2 | ((last$l1) == (last$l2)) = myIsSufixOf (take ((length l1)-1) l1) (take ((length l2)-1) l2)
                  | otherwise = False

--ou 
aux3::Eq a=> [a]->[a]->Bool
aux3 [] _ = True
aux3 _ [] = False
aux3 l1@(x:xs) l2@(y:ys) | (x==y) = aux3 xs ys
                         | otherwise = False


myIsSufixOf2::Eq a=> [a]->[a]->Bool
myIsSufixOf2 l1 l2 = aux3 (reverse l1) (reverse l2)



--18)
myIsSubSequenceOf::Eq a=>[a]->[a]->Bool
myIsSubSequenceOf [] _ = True
myIsSubSequenceOf _ [] = False
myIsSubSequenceOf l1@(x:xs) l2@(y:ys) | (x == y) = myIsSubSequenceOf xs ys
                                      | otherwise = myIsSubSequenceOf l1 ys



--19)
aux5::Eq a=> a ->Int ->[a]->[Int]
aux5 c i [] = []
aux5 c i l@(x:xs) | (x==c) = i:(aux5 c (i+1) xs)
                  | otherwise = aux5 c (i+1) xs 

myElemIndices::Eq a=>a->[a]->[Int]
myElemIndices c l = aux5 c 0 l


--20)
myNub::Eq a=>[a]->[a]
myNub [] = []
myNub l@(x:xs) = x:myNub (filter (/=x) l)



--21)
myDelete::Eq a=>a->[a]->[a]
mydelete c [] = []
myDelete c l@(x:xs) | (x==c) =  xs
                    | otherwise = x:myDelete c xs 


--22)
funcao22::Eq a=> [a]->[a]->[a]
funcao22 [] [] = []
funcao22 l [] = l
funcao22 l1@(x:xs) l2@(y:ys) = funcao22 (myDelete y l1) ys 


--23)
--IDEIA ALTERNATIVA: Pode se juntar as duas listas e remover os repetidos

--ou
existe::Eq a=> a->[a]->Bool
existe c [] = False
existe c l@(x:xs) | (x==c) = True
                  | otherwise = existe c xs

myUnion::Eq a=>[a]->[a]->[a]
myUnion [] l = l
myUnion l [] = l
myUnion l1@(x:xs) l2@(y:ys) | (existe y l1) = myUnion l1 ys
                            | otherwise = myUnion (l1++[y]) ys



--24)
myIntersect::Eq a => [a]->[a]->[a]
myIntersect [] [] = []
myIntersect [] l = []
myIntersect l1@(x:xs) l2@(y:ys) | (existe x l2) = x:myIntersect xs l2
                                | otherwise = myIntersect xs l2

--25)
myInsert::Ord a=> a->[a]->[a]
myInsert n [] = [n]
myInsert n l@(x:xs) | (n>=x) = x:myInsert n xs
                    | otherwise = n:x:xs



--26)
myUnWords::[String]->String
myUnWords [] = []
myUnWords [x] = x
myUnWords l@(x:xs) = x++" "++(myUnWords xs)



--27)
myUnlines::[String]->String
myUnlines [] = []
myUnlines l@(x:xs) = x++"\n"++myUnlines xs



--28)
aux6 ::Ord a=> Int->a->[a]->Int
aux6 i n [] = i
aux6 i n l@(x:xs) | (n>=x) = aux6 i n xs
                  | otherwise = aux6 (i+1) x xs


pMaior::Ord a=>[a]->Int
pMaior l@(x:xs) = aux6 0 x l


--29)
aux7::Eq a=> a -> [a]->Int
aux7 c [] = 0
aux7 c l@(x:xs) | (x==c) = 1+aux7 x xs
                | otherwise = aux7 c xs

temRepetidos::Eq a=>[a]->Bool
temRepetidos [] = False
temRepetidos l@(x:xs) | ((aux7 x l)>1) = True
                      | otherwise = temRepetidos xs


--30)
algarismos::[Char]->[Char]
algarismos [] = []
algarismos l@(x:xs) | (x>='0' && x<='9') = x:algarismos xs
                    | otherwise = algarismos xs


--31)
posImpares::[a]->[a]
posImpares [] = []
posImpares [x] = []
posImpares (x:y:xs) = y:posImpares xs 



--32)
posPares::[a]->[a]
posPares [] = []
posPares [x] = [x]
posPares (x:y:xs) = x:posPares xs



--33)
isSorted::Ord a=>[a]->Bool
isSorted [x] = True
isSorted (x:y:xs) | (x<=y) = isSorted (y:xs)
                  | otherwise = False


--34)
mYiSort::Ord a=>[a]->[a]
mYiSort [] = []
mYiSort l@(x:xs)= myInsert x (mYiSort xs)


--35)
menor::String->String->Bool
menor [] [] = False
menor _ [] = False
menor [] _ = True
menor l@(x:xs) l2@(y:ys) | (x<=y) = menor xs ys
                         | otherwise = False


--36)
myElemMSet::Eq a => a->[(a,Int)]->Bool
myElemMSet c [] = False
myElemMSet c l@((a,b):xs) | (a==c) = True
                          | otherwise = myElemMSet c xs


--37)
myLengthMSet::[(a,Int)]->Int
myLengthMSet [] = 0
myLengthMSet l@((a,b):xs) = b+myLengthMSet xs


--38)
constroi::a->Int->[a]
constroi x i | (i>0) = x:constroi x (i-1)
             | otherwise = [] 

myConvertMSet::[(a,Int)]->[a]
myConvertMSet [] = []
myConvertMSet l@((a,i):xs) = (constroi a i)++myConvertMSet xs 


--39)
myInsereMSet::Eq a => a->[(a,Int)]->[(a,Int)]
myInsereMSet c [] = [(c,1)]
myInsereMSet c l@((a,i):xs) | (a==c) = (a,i+1):xs
                            | otherwise = (a,i):myInsereMSet c xs


--40)
myRemoveMSet::Ord a => a->[(a,Int)]->[(a,Int)]
myRemoveMSet c [] = []
myRemoveMSet c l@((a,i):xs) | (a==c) && (i>1) = (a,(i-1)):xs
                            | (a==c) && (i==1) = xs
                            | otherwise = (a,i):myRemoveMSet c xs



--41)
constroiMSet::Ord a=> [a]->[(a,Int)]
constroiMSet [] = []
constroiMSet str@(x:xs) = (x, length (filter (==x) str)):constroiMSet (filter (/=x) str)

--42)
catMaybes::[Maybe a]->[a]
catMaybes [] = []
catMaybes l@(Just a:as) = a:catMaybes as
catMaybes l@(Nothing:as) = catMaybes as                        



--43)
partitionEithers::[Either a b]->([a],[b])
partitionEithers l = (esq l, dir l)
     where esq(Left a:xs) = a:esq xs
           esq(Right b:xs) = esq xs
           esq _= []
           dir(Right b:xs) = b:(dir xs)
           dir(Left a:xs) = (dir xs)
           dir _ = []





--44)
data Movimento = Norte | Sul | Este| Oeste
               deriving (Show,Eq)


posicao::(Int,Int)->[Movimento]->(Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) l@(ordem:xs) | (ordem==Norte) = posicao (x,(y+1)) xs
                           | (ordem==Sul) = posicao (x,(y-1)) xs
                           | (ordem==Este) = posicao ((x+1),y) xs
                           | (ordem==Oeste) = posicao ((x-1),y) xs
                           | otherwise = (x,y)


--45)
caminho::(Int,Int)->(Int,Int)->[Movimento]
caminho (xi,yi) (xf,yf) | (xi<xf) = Este:caminho (xi+1,yi) (xf,yf)
                        | (xi>xf) = Oeste:caminho (xi-1,yi) (xf,yf)
                        | (yi<yf) = Norte:caminho(xi,yi+1) (xf,yf)
                        | (yi>yf) = Sul:caminho (xi,yi-1) (xf,yf)
                        | otherwise = []


--46)
vertical::[Movimento]->Bool
vertical [] = True
vertical l@(ordem:xs) | (ordem==Norte) || (ordem==Sul) = vertical xs
                      | otherwise = False


--47)
data Posicao = Pos Int Int 
             deriving (Show,Eq)


distancia::Int->Int->Float
distancia x y = sqrt(fromIntegral(x*x) + fromIntegral(y*y))

procura::Int->Int->[Posicao]->Posicao
procura  x y [] = Pos x y
procura  x y l@((Pos a b):xs) | ((distancia x y)>(distancia a b)) = procura a b xs
                              | otherwise = procura x y xs 


maisCentral::[Posicao]->Posicao
maisCentral l@((Pos x y):xs) = procura x y xs


{--48)
confirma::Int->Int->Int->Int->Bool
confirma x y xx yy | (((xx==x-1) || (xx==xx) || (xx==x+1)) && (yy==y-1) = True 
                   | (((xx==x-1) || (xx==xx) || (xx==x+1)) && (yy==y+1) = True

vizinhos::Posicao->[Posicao]->[Posicao]
vizinhos Pos x y [] = []
vizinhos Pos x y l@((Pos a b):xs) | (confirma x y a b) = (Pos a b):vizinhos Pos x y xs
                                  | otherwise = vizinhos Pos x y xs


data Semaforo = Verde|Amarelo|Vermelho
	deriving Show
--}


