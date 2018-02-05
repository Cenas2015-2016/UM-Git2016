module Ficha9 where
--Exercicio 1
data Frac = F Integer Integer


--b)

instance Show Frac where
    show (F x y) = show x ++ "/" ++ show y

--c)

instance Eq Frac  where
(===) (F x y) (F z w) | ((x==z) && (y==w)) = True
                      | otherwise = False

--d)
instance Ord Frac where
(-<-) (F x y) (F z w) | (div x y) < (div z w) = True
                      | (((div x y) == (div z w)) && ((mod x y)<(mod z w))) = True
                      | otherwise = False

--Exercicio 2

data ExpInt = Const Int --este data type teve que ser ligeiramente modificado
            |Simetrico (ExpInt)
            |Mais (ExpInt) (ExpInt)
            |Menos (ExpInt) (ExpInt)
            |Mult (ExpInt) (ExpInt)


--a)
instance Show ExpInt where 
    show (Const a) = show a
    show (Simetrico a) = "-"++ show a
    show (Mais a b) = " (" ++ show a ++ "+" ++ show b ++ ") "
    show (Menos a b) = " (" ++ show a ++ "-" ++ show b ++ ") "
    show (Mult a b) = " (" ++ show a ++ "x" ++ show b ++ ") "
