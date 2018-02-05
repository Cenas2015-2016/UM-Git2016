module Exame14FEV2014 where

type Mat a = [[a]]
{--
verificaLinha::(Num a,Eq a) => [a]->Int->Bool
verificaLinha _ 0 = True
verificaLinha (x:xs) n | (x==0) = verificaLinha xs (n-1)
                       | otherwise = False


aux::(Num a,Eq a) => Mat a -> Int -> Int ->Bool
aux (x:xs) 1 _ = True
aux (x:xs) n t | (verificaLinha x t) = aux xs (n-1) t
               | otherwise = False

triSup::(Num a,Eq a) => Mat a -> Bool
triSup [x] =True
triSup (x:xs) = aux (reverse xs) (length (x:xs)) ((length x)-1)
              



--triSup::(Num a,Eq a) => Mat a -> Bool
--triSup (x:xs) = aux (reverse xs) (length (x:xs)) ((length x)-1)
-} 

verifLinha :: (Num a,Eq a) => [a] -> Int -> Bool
verifLinha l 0 = True
verifLinha l@(x:xs) n | (x==0) = verifLinha xs (n-1)
                      | otherwise = False

aux :: (Num a,Eq a) => Mat a -> Int -> Bool
aux [] _ = True
aux mat@(y:ys) p | (verifLinha y p) = aux ys (p+1)
                 | otherwise = False


triSup :: (Num a,Eq a) => Mat a -> Bool
triSup [] = True
triSup matriz@(x:xs) = aux matriz 0 

rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft mat@(x:xs) = (criaLinha mat): rotateLeft((map (drop 1) mat))
                    where
                        criaLinha :: Mat a -> [a]
                        criaLinha [] = []
                        criaLinha (x:xs) = (take 1 x)++criaLinha xs

data Questionario = Solucao String
                  | Questao String Questionario Questionario

q = Questao "a >= b"
        (Questao "a >= c"
            (Questao "b >= c"
                (Solucao "a b c")
                (Solucao "a c b"))
            (Solucao "c a b"))
    (Questao "a >= c"
        (Solucao "b a c")
        (Questao "b >= c"
            (Solucao "b c a")
            (Solucao "c b a")))

instance Show Questionario where
    show (Questao x q1 q2) = show x ++ "? NAO " ++ show q1 ++ "? SIM " ++ show q2 ++"\n"
    show (Questao x r@(Solucao y) q2) =  show x ++ show r ++ show q2  ++ "\n"
    show (Questao x q1 r@(Solucao y)) =  show x ++ show q1 ++ show r ++ "\n"
    show (Solucao x) = "SOLUCAO" ++ show x ++ "\n"

