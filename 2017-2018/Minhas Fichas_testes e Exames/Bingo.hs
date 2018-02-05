module Bingo where
 
bingo::IO()

bingo = do  
    x<-ramdomRIO(1,90)
    getchar
    return x
