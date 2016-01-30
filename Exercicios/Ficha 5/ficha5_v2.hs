import Data.Char
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)
conta :: Int -> Polinomio -> Int
conta n l = length(filter (\(x,y)->(n==y)) l)

--b)
grau :: Polinomio -> Int
grau l = maximum (map(\(x,y)->y) l)

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n l = filter(\(x,y) -> y==n) l

--d)
deriv :: [(Float,Float)] -> [(Float,Float)]
deriv p = map(\(x,y)->((x*y),(y-1))) p

--e)
calcula :: Float -> Polinomio -> Float
calcula s l = foldr (+) 0 (map(\(x,y)->((x*s)^y)) l)

--f)
simp :: Polinomio -> Polinomio
simp l = filter (\(x,y)->(x==0)) l

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) p = map (\(a,b)->((c*a),(e+b))) p

--h)
normaliza :: Polinomio->Polinomio
normaliza [] = []
normaliza l@((a,b):xs) = (sumMonomios(agrupaMonomios l)):normaliza lista
                         where lista = (filter (\(x,y)->(y/=b)) xs)

sumMonomios :: Polinomio -> Monomio
sumMonomios l@((a,b):xs) = (newa,b)
                        where lista = agrupaMonomios l
                              newa = (foldr (+) 0 (map (fst) lista)) --o map cria uma lista apenas com o1º elemento de cada tuplo e de seguida o foldr (+) 0  
                                                                     -- faz a respetiva soma dos mesmos

agrupaMonomios::Polinomio->Polinomio
agrupaMonomios l@((a,b):xs) = filter(\(x,y)->(y==b)) l


--i)
soma::Polinomio->Polinomio->Polinomio
soma l1 l2 = normaliza (l1++l2)

--j)
produto::Polinomio->Polinomio->Polinomio
produto l1 l2 = normaliza (aux l1 l2)
                where aux _ [] = []
                      aux [] _ = [] 
                      aux l1@((x,y):xs) l2@((a,b):ys) = ((mult (x,y) l2)++(aux xs l2))

--k)
--Ordenação de MSets com recurso apenas a ordem superior - FUNCIONAL!
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena l@((x,y):xs) = (ordena (filter(\(a,b)->(b<=y)) xs)) ++ [(x,y)] ++ (ordena (filter(\(a,b)->(b>y)) xs))




--Faz a manipulação dos Msets mas não ordena corretamente!
ordena1 :: Polinomio -> Polinomio
ordena1 [] = []
ordena1 l@((x,y):xs) = let (menores, maiores) = parte (x,y) xs
                      in (ordena1 menores)++[(x,y)]++(ordena1 maiores)

parte :: Monomio->Polinomio->(Polinomio,Polinomio)
parte (x,y) [] = ([],[])
parte (x,y) l@((a,b):xs) = let (maiores,menores) = parte (x,y) xs
                           in if (a<=x) then ((a,b):menores,maiores)
                                        else (menores,(a,b):maiores)


--l)



--Exercicio 2
nzp :: [Int] -> (Int,Int,Int)
nzp l@(x:xs) = ((length(filter (<0) l)),(length(filter (==0) l)),(length(filter (>0) l)))


--Exercicio 3
digitAlpha :: String -> (String,String)
digitAlpha l@(x:xs) = (a,b)
                        where a = filter (\(x)->(isDigit x)==True) l
                              b = filter (\(x)->(isAlpha x)==True) l


--Exercicio 6
--a)
zzipWith ::

