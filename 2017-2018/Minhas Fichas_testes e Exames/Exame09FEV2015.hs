module Exame09FEV2015 where

type TurmaL =  [(Numero,Aluno)]
type Aluno = (Nome,Nota)
type Numero = Int
type Nome = String
type Nota = Float



tL = [(1,("Joao", 12.3)),
      (3, ("Maria",5.4)),
      (6, ("Joana",9.5)),
      (12, ("Anastacia",18.8))]

--Exercicio 1
--a)
taxaAp :: TurmaL -> Float
taxaAp [] = 0
taxaAp t@(x:xs) = aux t 0 0
                where 
                    aux::TurmaL->Float->Float->Float
                    aux [] nAp nAval = (nAp/nAval)
                    aux t@((n,al):xs) nAp nAval | ((getNota al)>=9.5) = aux xs (nAp+1) (nAval+1)
                                                | otherwise = aux xs nAp (nAval+1)
                                                where
                                                    getNota::Aluno->Nota
                                                    getNota aluno@(n,nota) = nota
--b)
{--
getNumero::Aluno->Numero
getNumero al = 
myISort::Aluno->TurmaL->TurmaL
myISort al [] = [getNumero al,al]
myISort al t@(x:xs) | (snd al)>=(getNota(snd x)) = x:myISort al xs
                    | otherwise = al:t
                    where
                        getNota::Aluno->Nota
                        getNota (_,nota) = nota
-}

--c)
getNomes::TurmaL->[String]
getNomes [] = []
getNomes t@((_,al):xs) = (fst al):getNomes xs
{--
lNomeMax::TurmaL->Int
lNomeMax t = maximum (map (length) (map (getNomes) t))
-}


aluno1 = ("aa",101)
aluno2 = ("bb",112)
aluno3 = ("cc",122)
aluno4 = ("dd",132)

turma = [(2,aluno1),(3,aluno2),(4,aluno3),(5,aluno4)]

--d)
printAluno::TurmaL -> [String]
printAluno [] = []
printAluno ((n,al):xs) = ("Numero:" ++ show n ++ "  Nome:" ++ (fst (al)) ++ "  Classificacao:") : printAluno xs


listaT :: TurmaL -> IO()
listaT t = do {
                putStrLn listagem;
              }
            where
                listagem = unlines (printAluno t)


data TurmaA = Al (Numero,Aluno)
            | Fork (Numero,Numero) TurmaA TurmaA

tA = Fork (1,12) -- folhas entre 1 e 12
        (Fork (1,3) -- folhas entre 1 e 3
            (Al (1, ("Joao",12.3)))
            (Al (3, ("Maria", 5.4))))
        (Fork (6,12) -- folhas entre 6 e 12
            (Al (6, ("Joana", 9.5)))
            (Al (12,("Anastacia",18.8))))

toList::TurmaA->TurmaL
toList (Al (numero,aluno)) = [(numero,aluno)]
toList (Fork (_,_) t1 t2) = toList t1 ++ toList t2


lookupA::Numero->TurmaA->Maybe Aluno
lookupA numero turma | (existe numero turma) = procura numero turma
                     | otherwise = Nothing
                     where
                        existe::Numero->TurmaA->Bool
                        existe n (Al (numero,aluno)) | (numero==n) = True
                                                     | otherwise = False
                        existe n (Fork (_,_) t1 t2) = existe n t1 || existe n t2
                        procura :: Numero->TurmaA->Maybe Aluno
                        procura n (Al (numero,aluno)) = Just aluno
                        procura n (Fork (x,y) t1 t2) | (n==x) = procura n t1
                                                     | (n==y) = procura n t2
                                                     | (n<(div y 2)) = procura n t1
                                                     | otherwise = procura n t2


remove::TurmaA->Numero->Maybe TurmaA
remove t@(Fork (x,y) t1 t2) numero | (existe numero t) = Just (retira numero t)
                                   | otherwise = Just (t)
        where
            existe num (Al (x,_)) | ( x==num) = True
            existe num (Fork (a,b) t1 t2) = existe num t1 || existe num t2
            retira num (Fork (a,b) t1 t2) | (a==num) = (Fork (b,b) t1 t2)
                                          | (b==num) = (Fork (a,a) t1 t2)
                                          | (num<(div b 2)) = (Fork (a,b) (retira num t1) t2)
                                          | otherwise = (Fork (a,b) t1 (retira num t2))




instance Show TurmaA where
    show (Al (numero,al)) = "  Al " ++ "(" ++ show numero ++ ", (" ++ show (fst al) ++ ", " ++ show (snd al) ++ ")\n"
    show (Fork (numero1,numero2) t1 t2) = "Fork (" ++ show numero1 ++ "," ++ show numero2 ++ ")\n" ++ "  " ++ show t1 ++ "  " ++ show t2

