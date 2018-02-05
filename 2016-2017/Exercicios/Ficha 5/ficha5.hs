type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Conta quantos monómios de grau n existem num polinómio p.
conta :: Int -> Polinomio -> Int
conta n p = length (filter (\(c,e) -> e == n) p)

-- Função que indica o grau de um polinómio.
grau :: Polinomio -> Int
grau p = foldl1 (\x y -> max x y) (map (snd) p)

-- Função que seleciona os monómios de grau n.
selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\(c,e) -> e == n) p

-- Função que deriva um polinómio.
deriv :: [(Float,Float)] -> [(Float,Float)]
deriv p = map (\(c,e) -> (c*e, e-1)) p

-- Função que calcula um polinómio para x == n.
calcula :: Float -> Polinomio -> Float
calcula n p = foldr (+) 0 (map (\(c,e) -> c*(n^e)) p)

-- Função que simplifica um polinómio (isto é, retira monómios de coeficiente zero.
simp :: Polinomio -> Polinomio
simp p = filter (\(c,e) -> c /= 0) p

-- Função que multiplica um monómio por um polinómio.
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) p = map (\(x,y) -> (c*x,e+y)) p

-- Função que dado um polinómio, constroi um equivalente, em que não aparecem vários monómios do mesmo grau.
normaliza :: Polinomio -> Polinomio
normaliza p = aux (agrupaMonomios p)
              where aux [] = []
                    aux (h:t) = ((foldr1 (+) (map (fst) h)),(snd(head h))) : aux t

-- Função que agrupa monómios do mesmo grau.
agrupaMonomios :: Polinomio -> [Polinomio]
agrupaMonomios [] = []
agrupaMonomios p@((c,e):t) = (filter (\(x,y) -> y == e) p) : agrupaMonomios (filter (\(x,y) -> y /= e) p)

-- Função que soma dois polinómios.
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza(p1 ++ p2)

-- Função que calcula o produto de dois polinómios.
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = reverse(ordena(normaliza(concat (aux p1 p2))))
                where aux [] _ = []
                      aux (h:t) p2 = mult h p2 : aux t p2
                      
-- Função que ordena um polinómio por ordem crescente de grau (Insertion Sort).
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (x:xs) = ins x (ordena xs)
              where ins x [] = [x]
                    ins x (b:bs) |(snd x <= snd b) = x : b : bs
                                 |otherwise = b : (ins x bs)

-- Função que verifica se dois polinómios são equivalentes.
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = (calcula 2 p1 == calcula 2 p2)
