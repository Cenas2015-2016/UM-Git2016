type TabClass = [(Piloto,Equipa,Pontos)]
type Piloto = String
type Pontos = Int
type Equipa = String

-- Função que calcula o número total de pontos arrecadados por uma dada equipa. 
pontosEquipa :: Equipa -> TabClass -> Pontos
pontosEquipa team l = foldr (+) 0 b
                    where b = (map (trd) c)
                          c = (filter (\(p,e,pts) -> e == team) l)
                          trd (x,y,z) = z

-- Função que recebe uma lista dos dez primeiros classificados e atualiza tabela de classificação geral.          
junta :: [Piloto] -> TabClass -> TabClass
junta p l = updated ++ others
            where updated = zipWith (\(a,b,_) x -> (a,b,x)) marked [25,18,15,12,10,8,6,4,2,1] 
                  marked = filter (\(x,y,z) -> elem x p) l
                  others = filter (\(x,y,z) -> not (elem x p)) l

-- Função que ordena por ordem decrescente a tabela de classificações geral.
ordena :: TabClass -> TabClass
ordena l = reverse (iSort l)

-- Algoritmo de ordenação Insertion Sort, utilizado na função ordena.
iSort :: TabClass -> TabClass
iSort [] = []
iSort (x:xs) = ins x (iSort xs)
              where trd (x,y,z) = z
                    ins x [] = [x]
                    ins x (b:bs) |(trd x <= trd b) = x : b : bs
                                 |otherwise = b : (ins x bs)
