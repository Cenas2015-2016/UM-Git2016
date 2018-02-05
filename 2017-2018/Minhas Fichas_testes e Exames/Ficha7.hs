module Ficha7 where

data BTree a = Empty
             | Node a (BTree a) (BTree a)
         deriving Show



--Exercicio 1
arvore = (Node 1 (Node 2 (Node 3 Empty (Node 4 Empty Empty)) Empty) (Node 5 Empty (Node 6 Empty Empty)))
listPath = [False,False,False,True,True]
listPath2 = [True]
--a)
altura::BTree a -> Int
altura Empty = 0
altura (Node _ btreel btreer) = 1 + (max (altura btreel) (altura btreer)) 


--b)
contaNodos::BTree a -> Int
contaNodos Empty = 0
contaNodos (Node _ btreel btreer) = 1 + (contaNodos btreel) + (contaNodos btreer)

--c)
folhas::BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ btreel btreer) = (folhas btreel) + (folhas btreer)

--d)
prune::Int->BTree a->BTree a
prune n Empty = Empty
prune 0 (Node a btreel btreer) = (Node a Empty Empty)
prune n (Node a btreel btreer) = (Node a (prune (n-1) btreel) (prune (n-1) btreer))

--e)
path::[Bool]->BTree a -> [a] 
path [] Empty = []
path _ Empty = []
path [] (Node v btreel btreer) = [v]
path l@(x:xs) (Node v btreel btreer) | (x==False) = v:(path xs btreel)
                                     | otherwise = v:(path xs btreer)

--f)
mirror::BTree a -> BTree a
mirror Empty = Empty
mirror (Node v btreel btreer) = (Node v (mirror btreer) (mirror btreel))


--g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node a b1l b1r) (Node b b2l b2r) = (Node (f a b) (zipWithBT f b1l b2l) (zipWithBT f b1r b2r))







--h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) btreel btreer) = (Node a (unzipA btreel) (unzipA btreer),
                                        Node b (unzipB btreel) (unzipB btreer),
                                        Node c (unzipC btreel) (unzipC btreer))
        where unzipA Empty = (Empty)
              unzipA (Node (a,b,c) btreel btreer) = (Node a (unzipA btreel) (unzipA btreer))
              unzipB Empty = (Empty)
              unzipB (Node (a,b,c) btreel btreer) = (Node b (unzipB btreel) (unzipB btreer))
              unzipC Empty = (Empty)
              unzipC (Node (a,b,c) btreel btreer) = (Node c (unzipC btreel) (unzipC btreer))



--Exercicio 2
arvoreBin = (Node 1 (Node (-5) (Node (-7) Empty Empty) (Node (-4) Empty Empty)) (Node 2 Empty (Node 8 (Node 7 Empty Empty) (Node 10 (Node 9 Empty Empty) Empty))))

--a)
minimo::Ord a => BTree a -> a
minimo (Node val Empty Empty) = val
minimo (Node val Empty btrR) = val
minimo (Node val btrL btrR) = minimo btrL

--b)
semMinimo::Ord a => BTree a ->BTree a
semMinimo Empty = Empty
semMinimo (Node a btrL@(Node val Empty Empty) btrR) = (Node a Empty btrR)
semMinimo (Node a btrL btrR) = (Node a (semMinimo btrL) btrR)



--c) - Não foi feito por mim e acho que está mal pois nao elimina o menor elemento da arvore
minSmin' :: (Ord a) => (BTree a) -> (a,BTree a)
minSmin' (Node x Empty rnode) = (x, rnode)
minSmin' (Node x lnode rnode) = let (a,b) = minSmin' lnode in (a, Node x b rnode)



--Exercicio 3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Integer
type Nome = String
data Regime = ORD 
            | TE 
            | MEL 
            deriving Show
data Classificacao = Aprov Float
                   | Rep
                   | Faltou
                   deriving Show

type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)


aluno1 = (10,"Luis",ORD,Rep)
aluno5 = (20,"Gonçalo",ORD,Faltou)
aluno4 = (14,"Patricia",TE,Aprov 12)
aluno2 = (13,"Rodrigo",MEL,Aprov 17)
aluno3 = (15,"João",TE,Rep)
aluno6 = (25,"Andreia",ORD,Aprov 11)


turma=(Node (10,"Luis",ORD,Rep) Empty (Node (20,"Gonçalo",ORD,Faltou) (Node (14,"Patricia",TE,Aprov 12) (Node (13,"Rodrigo",MEL,Aprov 17) Empty (Node (15,"João",TE,Rep) Empty Empty)) Empty) (Node (25,"Andreia",ORD,Aprov 11) Empty Empty)))
--ou
turma2=(Node aluno1 Empty (Node aluno5 (Node aluno4 (Node aluno2 Empty (Node aluno3 Empty Empty)) Empty) (Node aluno6 Empty Empty)))



--a)
inscNum::Numero->Turma->Bool
inscNum _ Empty = False
inscNum n (Node aluno@(num,_,_,_) btrL btrR) | (num==n) = True
                                             | (n>num)= inscNum n btrR 
                                             | otherwise = inscNum n btrL

--b)
inscNome::Nome->Turma->Bool
inscNome _ Empty = False
inscNome nome (Node aluno@(_,name,_,_) btrL btrR) | (nome == name) = True
                                                  | otherwise = ((inscNome nome btrL) || (inscNome nome btrR))




--c)
ordenaPNum::[(Numero,Nome)]->[(Numero,Nome)]
ordenaPNum [] = []
ordenaPNum (x:xs) = myIsort x (ordenaPNum xs)
                  where
                    myIsort x [] = [x]
                    myIsort x (y:ys) | ((fst$x)>(fst$y)) = y:(myIsort x ys)
                                     | otherwise = x:y:ys

trabEst::Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst turma = ordenaPNum(listPreorder turma)
              where
                listPreorder Empty = []
                listPreorder (Node (num,nome,TE,_) btreeL btreeR) = [(num,nome)] ++ (listPreorder btreeL) ++ (listPreorder btreeR) 
                listPreorder (Node _ btreeL btreeR) = (listPreorder btreeL) ++ (listPreorder btreeR)


--d)
nota::Numero->Turma->Maybe Classificacao
nota n Empty = Nothing
nota n (Node (num,_,_,clsfc) btreeL btreeR) | (n<num) = nota n btreeL
                                            | (n>num) = nota n btreeR
                                            | otherwise = Just clsfc


--e)
numAlunos::Turma->Float
numAlunos Empty = 0
numAlunos (Node _ btreeL btreeR) = 1 + (numAlunos btreeL) + (numAlunos btreeR)

numAlunosCFaltas::Turma->Float
numAlunosCFaltas Empty = 0
numAlunosCFaltas (Node (_,_,_,Faltou) btreeL btreeR) = 1 + (numAlunosCFaltas btreeL) + (numAlunosCFaltas btreeR)
numAlunosCFaltas (Node (_,_,_,_) btreeL btreeR) = (numAlunosCFaltas btreeL) + (numAlunosCFaltas btreeR)

percFaltas::Turma->Float
percFaltas Empty = 0
percFaltas turma = ((numAlunosCFaltas turma)/(numAlunos turma))*100

--OU

percFaltas'::Turma->Float
percFaltas' Empty = 0
percFaltas' turma = ((numAlunosCFaltas' turma)/(numAlunos' turma))*100
                 where
                    numAlunosCFaltas' Empty = 0
                    numAlunosCFaltas' (Node (_,_,_,Faltou) btreeL btreeR) = 1 + (numAlunosCFaltas' btreeL) + (numAlunosCFaltas' btreeR)
                    numAlunosCFaltas' (Node (_,_,_,_) btreeL btreeR) = (numAlunosCFaltas' btreeL) + (numAlunosCFaltas' btreeR)
                    numAlunos' Empty = 0
                    numAlunos' (Node _ btreeL btreeR) = 1 + (numAlunos' btreeL) + (numAlunos' btreeR)

--f)
numAprov::Turma->Float
numAprov Empty = 0
numAprov (Node (_,_,_,Aprov x) btreeL btreeR) = 1 + (numAprov btreeL) + (numAprov btreeR) 
numAprov (Node (_,_,_,_) btreeL btreeR) = (numAprov btreeL) + (numAprov btreeR)


mediaAprov::Turma->Float
mediaAprov Empty = 0
mediaAprov turma = (somaNotasAp turma)/(numAprov turma)
              where
                somaNotasAp Empty = 0
                somaNotasAp (Node (_,_,_,Aprov x) btreeL btreeR) = x + (somaNotasAp btreeL)
                                                                     + (somaNotasAp btreeR)
                somaNotasAp (Node (_,_,_,_) btreeL btreeR) =  (somaNotasAp btreeL) +
                                                              (somaNotasAp btreeR)


--g) ---NAO ESTÁ A FAZER APENAS UMA TRAVESSIA
aprovAv::Turma->Float
aprovAv Empty = 0
aprovAv turma = ((numAprov turma)/(numAval turma))*100
              where
                numAval Empty = 0
                numAval (Node (_,_,_,Aprov x) btreeL btreeR) = 1 + (numAval btreeL) + (numAval btreeR)
                numAval (Node (_,_,_,Rep) btreeL btreeR) = 1 + (numAval btreeL) + (numAval btreeR)
                numAval (Node _ btreeL btreeR) = (numAval btreeL) + (numAval btreeR)


--OU
--Esta resolução nao foi feita por mim
aprovAv' :: Turma -> Float
aprovAv' t = let (a,b) = aAAux t in a / b where
                                            aAAux Empty = (0,0)
                                            aAAux (Node (_, _, _, Aprov x) lnode rnode) = (1 + al + ar, 1 + bl + br) where
                                                                            (al,bl) = aAAux lnode
                                                                            (ar,br) = aAAux rnode
                                            aAAux (Node (_, _, _, Rep) lnode rnode) = (0 + al + ar, 1 + bl + br) where
                                                                            (al,bl) = aAAux lnode
                                                                            (ar,br) = aAAux rnode
                                            aAAux (Node _ lnode rnode) = (0 + al + ar, 0 + bl + br) where
                                                            (al,bl) = aAAux lnode
                                                            (ar,br) = aAAux rnode


{--

-------------EXTRA-------
--Acrescentar um nodo na arvore:
acrescenta Empty v = Node v Empty Empty
acrescenta (Node x e d) v | x > v = Node (acrescenta e v) d
                          | otherwise = Node e (acrescenta d v)



--PROCURAR UM EEMENTO NUMA ARVBIN
procura :: (Eq a) => a->a->BTree a -> Bool
procura x Empty = False
procura x (Node a left right) | (a==x) = True
                              | otherwise = (procura x left) || (procura x right) 


--INSERIR ELEMENTO NUMA ARVORE ORDENADA
insertABin::Ord a => a->BTree a -> BTree a
insertABin x Empty = (Node x Empty Empty)
insertABin x (Node a leftAB rightAB) | (x<a) = Node a (insertABin x leftAB) rightAB
                                     | otherwise = Node a leftAB (insertABin x rightAB)
-}
--ELIMINA UM ELEMENTO DA RAIZ
deleteAbin::Ord a=> a->BTree a -> BTree a
deleteAbin _ Empty = Empty
deleteAbin x (Node a left right) | (x<y) = Node (deleteAbin x left) right
                                 | (x>y) = Node left (deleteAbin x right)
                                 | otherwise = apagaRaiz (Node a left right)

apagaRaiz::Ord a => BTree a -> BTree a
apagaRaiz (Node x l Empty) = l
apagaRaiz (Node x l r) = Node (menor r) l (apagaMenor r)    

apagaMenor::Ord a=> BTree a ->BTree a
apagaMenor (Node x Empty r) = r
apagaMenor (Node x l r) = Node x (apagaMenor l) r


{--
PONTOS A REVER:
-ver como funciona o uso do let
-reformular o exe3 G) para que só percorra uma unica vez a arvore(necessidade do let)
-ver a resolução existente do exe2 c)(a resolução existente nao é minha) e entender melhor o raciocinio a aplicar
--a alinea d) exe2 não é bem explicita no que pede, dai nao ter feito
-}