--Listas usadas para testes
a = [(1,'a'),(2,'r'),(54,'b'),(1,'x')]
b = [(1,2,3),(4,3,2),(5,4,3),(9,76,1),(0,0,0),(1,2,3)]   


--sempre que possivel usar-se-á ordem superior

--Exercicio 2
--a)
segundos::[(a,b)]->[b]
segundos l = map (\(x,y)->y) l


--b)
nosPrimeiros::Eq a=> a -> [(a,b)]->Bool
nosPrimeiros x [] = False
nosPrimeiros x l@((a,b):xs) | x==a = True
                            | otherwise = nosPrimeiros x xs

--c)
minFst :: Ord a=>[(a,b)] -> a
minFst l@((a,b):xs) = menor a l

menor :: Ord a=> a -> [(a,b)] -> a
menor x [] = x
menor x ((a,b):xs) | (a<=x) = menor a xs
                   | otherwise = menor x xs

--d)
sndMinFst :: Ord a=> [(a,b)]->b
sndMinFst l@((a,b):xs) = menor2 (a,b) l

menor2 :: Ord a=> (a,b) -> [(a,b)] -> b
menor2 (x,y) [] = y
menor2 (x,y) ((a,b):xs) | (a<=x) = menor2 (a,b) xs
                        | otherwise = menor2 (x,y) xs

--e)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos l@((a,b,c):xs) = soma (a,b,c) xs

soma :: (Num a, Num b, Num c) =>(a,b,c) -> [(a,b,c)] -> (a,b,c)
soma (x,y,z) [] = (x,y,z)
soma (x,y,z) l@((a,b,c):xs) = soma (x+a,y+b,z+c) xs


--f)
--devolve ultimo elemento da lista das somas de triplos, que diz respeito ao maior valor obtido nas somas dos triplos (3º processo do problema)
maxTriplo ::(Ord a,Num a) => [(a,a,a)] -> a
maxTriplo l = last (ordenaLista (listaDasSomas l))

--quickSort que ordena por ordem crescente a lista que contem apenas os resultados das somas dos triplos (2º processo do problema)
ordenaLista ::(Ord a,Num a) => [a] -> [a]
ordenaLista [] = []
ordenaLista l@(h:t) = ordenaLista (filter (<=h) t) ++ [h] ++ ordenaLista (filter (>h) t)

--cria a lista apenas com os resultados das respetivas somas dos triplos (1º processo do problema)
listaDasSomas :: (Ord a,Num a) =>[(a,a,a)] -> [a]
listaDasSomas [] = []
listaDasSomas l@((x,y,z):xs) = (x+y+z):listaDasSomas xs



--Exercicio 3
--a)
multPorSumSucess::Int->Int->Int
multPorSumSucess x y | (y/=1) = x + multPorSumSucess x (y-1)
                     | otherwise = x 

--b)
--NAO ESTÁ FUNCIONAL
divisao :: Int->Int->Int
divisao x 0 = -1
divisao x y | ((mod x y)/=0) = 1 + divisao x (y-1)
            | otherwise =divisao x (y-1)


--c)
power::Int->Int->Int
power x 0 = 1
power x e = x*power x (e-1)


--Exercicio 4
type Hora = (Int,Int)
type Etapa = (Hora,Hora)
type Viagem = [Etapa]
c = [((9,30),(10,25)),((11,20),(12,45)),((13,30),(14,45))]

--a)
validaEtapa :: Etapa->Bool
validaEtapa ((a,b),(c,d)) | (a<c) = True
                          | ((a==c) && (b<d)) = True
                          | otherwise = False


--b)
validaViagem ::Viagem->Bool
validaViagem [] = False
validaViagem (((a,b),(c,d)):xs) = auxValida ((a,b),(c,d)) xs

auxValida::Etapa->Viagem->Bool
auxValida ((a,b),(c,d)) [] = True
auxValida ((a,b),(c,d)) (((h3,m3),(h4,m4)):xs) | (a<c) && (c<h3) = auxValida ((h3,m3),(h4,m4)) xs
                                               | (a==c) && (b<d) && (c==h3) && (d<m3) = auxValida ((h3,m3),(h4,m4)) xs
                                               | otherwise = False


--c)
horaPartCheg :: Viagem->Viagem
horaPartCheg l = [head l,last l]


--d)
tempoTotalEfec ::Viagem->IO()
{--
o MAP vai fazer com que se calcule para cada etapa o tempo de duração e o FOLDR vai no final fazer a devida soma 
de todos os tempos, o que correponde ao valor efetivo da viagem

            foldr (+) 0   ---> corre a lista e soma todos os elementos a partir do indice 0
--}
tempoTotalEfec l =  putStrLn (show h ++ " horas e " ++ show m ++ " minutos")
                    where h=(div (foldr (+) 0 (map (\(x,y) -> diferenca (x,y)) l)) 60)
                          m=(mod (foldr (+) 0 (map (\(x,y) -> diferenca (x,y)) l)) 60)
--calcula apenas o tempo que dura uma etapa 
diferenca:: Etapa->Int
diferenca ((h1,m1),(h2,m2)) | ((h2-h1)*60 < 0) = ((h2-h1)*60) * (-1) + (m2-m1) 
                            | otherwise = ((h2-h1)*60)+(m2-m1)


--e)
esperaTotal::Viagem->IO()
esperaTotal l = putStrLn (show h ++ " horas e " ++ show m ++ " minutos")
              where h = (div (foldr (+) 0 (diferenca2 l)) 60)
                    m = (mod (foldr (+) 0 (diferenca2 l)) 60)
diferenca2 :: Viagem -> [Int]
diferenca2 [] = []
diferenca2 [x] = []
diferenca2 (e1:e2:t) = diferenca (snd e1, fst e2) : diferenca2 (e2:t) 


--f)
tempoTotal ::Viagem-> IO()
tempoTotal l = putStrLn (show h ++ " horas e " ++ show m ++ " minutos")
                where h = div ((((fst$fst$last$l)-(fst$fst$head$l))*60)+((snd$snd$last$l)-(snd$snd$head$l))) 60
                      m = mod ((((fst$fst$last$l)-(fst$fst$head$l))*60)+((snd$snd$last$l)-(snd$snd$head$l))) 60