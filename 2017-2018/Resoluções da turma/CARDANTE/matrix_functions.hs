type Mat a = [[a]]

-- |Testa se uma matriz está bem construída.
dimOk :: Mat a -> Bool
dimOk matrix = filter (/= head l) l == [] 
               where l = map (length) matrix

-- |Calcula a dimensão de uma matriz (nr.colunas, nr.linhas).
dimMat :: Mat a -> (Int,Int)
dimMat l@(h:t) = (length h, length l)

-- |Adiciona duas matrizes.
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat m1@(h:t) m2@(x:xs) = (zipWith (+) h x) : addMat t xs 

-- |Calcula a transposta de uma matriz. 
transpose :: Eq a => Mat a -> Mat a
transpose [] = []
transpose l | elem False (map (==[]) l) = newLine : transpose (newList) -- corrige o problema da última passagem pela matriz, onde todas as linhas são [].
            | otherwise = []  
              where newList = map (tail) l 
                    newLine = map (head) l

-- |Multiplica duas matrizes. Assume que a multiplicação é possível (i.e., nr.colunas matriz 1 = nr.linhas matriz 2).
multMat :: Num a => Eq a => Mat a -> Mat a -> Mat a
multMat [] _ = []
multMat _ [] = [] 
multMat m1@(h:t) m2 = aux h (transpose m2) : multMat t m2


aux :: Num a => [a] -> Mat a -> [a]
aux [] _ = []
aux _ [] = []
aux firstrow m2@(x:xs) = (foldr (+) 0 (zipWith (*) firstrow x)) : aux firstrow xs

-- |Verifica se uma matriz é triangular superior.
triSup :: Eq a => Num a => Mat a -> Bool
triSup matrix = not (elem False condition) 
                where condition = (map (==0) (concat (aux2 (tail matrix) 0)))

aux2 :: Num a => Mat a -> Int -> Mat a
aux2 [] _ = []
aux2 (h:t) n = take (n+1) h : aux2 t (n+1)

-- |Roda uma matriz 90º para a esquerda.
rotateLeft :: Eq a => Mat a -> Mat a
rotateLeft l = transpose (map (reverse) l)