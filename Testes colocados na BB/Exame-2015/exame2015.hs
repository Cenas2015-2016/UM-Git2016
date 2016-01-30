type TurmaL = [(Numero,Aluno)]
type Aluno = (Nome,Nota)
type Numero = Int
type Nome = String
type Nota = Float

-- Calcula a taxa de aprovações numa turma.
taxaAp :: TurmaL -> Float
taxaAp l = positivas / total
           where positivas = foldr (\_ n -> n + 1) 0 (filter (>=9.5) notas)
                 total = foldr (\_ n -> n + 1) 0 notas
                 notas = map (snd . snd) l

-- Calcula os n melhores alunos de uma turma.             
top :: Int -> TurmaL -> [String]
top n l = take n (map (fst . snd) (reverse(sort l)))

--Calcula o comprimento do maior nome de um alunos de uma turma. 
lNomeMax :: TurmaL -> Int
lNomeMax l = foldl1 (\x y -> max x y) (map (length . fst . snd) l)

--Imprime no ecrã uma turma (número, aluno e nota).                                                                  
listaT :: TurmaL -> IO ()
listaT [] = error "Turma não introduzida!"
listaT l = putStrLn (unlines (map (updGrade) l))


----------------FUNÇÕES AUXILIARES--------------------------------------------------
-- Algoritmo de ordenação Insertion Sort
sort :: TurmaL -> TurmaL
sort [] = []
sort (x:xs) = ins x (sort xs)
              where ins x [] = [x]
                    ins x (b:bs) |(snd (snd x) <= snd (snd b)) = x : b : bs
                                 |otherwise = b : (ins x bs)

--Atualiza notas de turma e transforma cada par (Numero, Aluno) numa String.
updGrade :: (Numero,Aluno) -> String
updGrade (n,(name,grade)) 
        | (grade >= 9.5) = show n ++ " " ++ name ++ " " ++ ((show . round) grade)
        | otherwise = show n ++ " " ++ name ++ " " ++ "R"
------------------------------------------------------------------------------------
