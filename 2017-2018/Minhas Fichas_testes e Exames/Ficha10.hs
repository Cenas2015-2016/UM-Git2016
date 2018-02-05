module Ficha10 where
import Data.Char
import Data.List

data Aposta = Ap [Int] (Int,Int)



{--

bingo :: IO()
bingo = do bingi []

bingi :: [Int] -> IO()
bingi lista = do {
                      if length lista == 90 then do {
                                                     putStrLn "Já Saíram todos os números";
                                                     return()
                                                    }
                      else do {
                               n <- randomRIO (1,90);
                               if (elem n lista) then bingi lista
                               else do {
                                        putStrLn "Prima Enter";
                                        getChar ;
                                        print n;
                                        bingi (n:lista)
                                      }
                             }
                }

--Exercicio 2

sorteio::IO()
sorteio = do gerador [] (0,0)


gerador::([Int],(Int,Int) -> IO()
gerador (nums@(x:xs),stars@(a,b)) = do {

    if((length nums)==5){
        geraStars stars
        putStrln "Já se encontram sorteados os numeros do EuroBulhoes"
        return ()

    }else 

        do{

          x <- randomRIO (1,50);
          if (elem x lstNumeros) | gerador lstNumeros
                                 | otherwise = gerador (x:lstNumeros)

                             }

    }

-}
aposta1=(Ap [5,4,3,2,1] (3,2)) -----Valida
aposta2=(Ap [1,2,6,7,8] (2,1)) -----Valida
aposta3=(Ap [1,2,3,4,6,4] (1,2)) ---Invalida
aposta4=(Ap [6,5,4,3] (3,2)) -------Invalida
aposta5=(Ap [1,2,3,4,5] (0,1)) -----Invalida
aposta6=(Ap [1,2,3,4,5] (1,10)) ----Invalida
aposta7=(Ap [1,2,3,4,99] (1,2)) ----Invalida

--a)
valida::Aposta->Bool
valida (Ap l (a,b)) | (((length l) == 5) && (verifRange l) && (verifReps l) && (verifRangeStars (a,b))) = True
                    | otherwise = False
                    where 
                          verifReps :: [Int] ->Bool
                          verifReps [x] = True 
                          verifReps l@(x:xs) | (elem x xs) == True = False
                                             | otherwise = verifReps xs 
                          verifRange::[Int]->Bool
                          verifRange [] = True
                          verifRange l@(x:xs) | ((x>0) && (x<10)) = verifRange xs
                                              | otherwise = False
                          verifRangeStars::(Int,Int)->Bool
                          verifRangeStars (x,y) | (x/=y) && (x>0 && x<10) && (y>0 && y<10) = True
                                                | otherwise = False

--b)
comuns::Aposta->Aposta->(Int,Int)
comuns (Ap l1 (a,b)) (Ap l2 (c,d)) = (verifCom1 l1 l2,verifCom2 (a,b) (c,d))
                    where
                        verifCom1::[Int]->[Int]->Int
                        verifCom1 [] l2 = 0
                        verifCom1 l1@(x:xs) l2 | (elem x l2) = 1 + verifCom1 xs l2
                                               | otherwise = verifCom1 xs l2 
                        verifCom2::(Int,Int)->(Int,Int)->Int
                        verifCom2 (a,b) (c,d) = length (intersect (a:b:[]) (c:d:[]))


--c1)
instance Eq Aposta where 
(===) ap1 ap2 | ((comuns ap1 ap2)==(5,2))=True
              | otherwise = False     


--c2)
premio::Aposta->Aposta->Maybe Int
premio ap1 ap2 = definePremio(comuns ap1 ap2)
                where
                    definePremio::(Int,Int)->Maybe Int
                    definePremio (x,y) | ((x==5) && (y==2)) = Just 1
                                       | ((x==5) && (y==1)) = Just 2
                                       | ((x==5) && (y==0)) = Just 3
                                       | ((x==4) && (y==2)) = Just 4
                                       | ((x==4) && (y==1)) = Just 5
                                       | ((x==4) && (y==0)) = Just 6
                                       | ((x==3) && (y==2)) = Just 7
                                       | ((x==3) && (y==1)) = Just 8
                                       | ((x==3) && (y==0)) = Just 9
                                       | ((x==2) && (y==2)) = Just 10
                                       | ((x==2) && (y==1)) = Just 11
                                       | ((x==2) && (y==0)) = Just 12
                                       | ((x==1) && (y==2)) = Just 13
                                       | otherwise = Nothing


--d)
getInteger::IO Integer
getInteger = do s<-getLine
    if all isDigit s then
        return (read s) else getInteger


        
leAposta::IO Aposta
leAposta = do leitura [] (0,0)

leitura::[Int] -> (Int,Int) -> IO()
leitura l (x,y) = do{
                        if((length l)<5) then do{
                            printStrLn "Insira os numeros do EuroBulhoes";
                            val<-getInteger;
                            if (val<1 || val>50) then do{
                                printStrLn "Valor inserido invalido";
                                leitura l (x,y);
                            }
                            else do{
                                leitura (val:l) (x,y);
                            }
                            printStrLn "Valores do EuroBulhoes inseridos com exito";
                        }else do{
                            printStrLn "Insira o valor das estrelas";
                            starVal1<-getInteger;
                            starVal2<-getInteger;
                            if((starVal1<1 || starVal1>9) || (starVal2<1||starVal2>9)) then do{
                                printStrLn "Valores invalidos";
                                leitura l (x,y);
                            }
                            else do{
                                printStrLn "O seu EuroBulhoes foi registado com exito";
                                return(Ap l (starVal1,starVal2));
                            }
                            
                        }
                    }

