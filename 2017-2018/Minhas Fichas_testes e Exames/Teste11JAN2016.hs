module Teste11JAN2016 where

--EXE1
--a)
nub::Eq a => [a] -> [a]
nub [] = []
nub l@(x:xs) = x:nub(filter (/=x)l)


--b)
zipwhith'::(a->b->c)->[a]->[b]->[c]
zipwhith' f [] [] = []
zipwhith' f l [] = []
zipwhith' f [] l = []
zipwhith' f (x:xs) (y:ys) = (f x y):zipwhith' f xs ys


--EXE2
type MSet a = [(a,Int)]
msetT1 = [('a',2),('b',3)] 
msetT2 = [('c',6),('b',2),('a',5)]
--a
converte::Eq a =>[a]->MSet a
converte [] = []
converte l@(x:xs) = (x,aux x l):converte (filter(/=x) l)
                where
                    aux::Eq a=>a->[a]->Int
                    aux c [] = 0
                    aux c (x:xs) | (x==c) = 1 + aux c xs
                                 | otherwise = aux c xs



--b)
intersect::Eq a => MSet a -> MSet a -> MSet a
intersect l [] = []
intersect [] l = []
intersect (x:xs) (y:ys) = (aux x (y:ys))++(intersect xs (filter (/=x) (y:ys)))
                        where 
                            aux::Eq a => (a,Int) -> MSet a -> MSet a
                            aux p [] = []
                            aux p (x:xs) |((fst p)==(fst x)) = [(fst p, (min (snd p) (snd x)))]
                                         |otherwise = aux p xs



--EXE3
--a)
data Prop = Var String 
          | Not Prop 
          | And Prop Prop 
          | Or Prop Prop

p1 :: Prop
p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))
{--
class Show a where
    Show::a->String

instance Show a => Show (Prop a) where
Show (Var x) = x
Show (Not x) = "-" ++ Show x
Show (And x y) = " (" ++ Show x ++ "/\*" ++ Show y ++ ") " 
Show (And x y) = " (" ++ Show x ++ "\/" ++ Show y ++ ") "
-}

--b)
eval::[(String,Bool)]->Prop->Bool
eval l (Prop Not x) = Not (eval l x)
eval l (Prop And x y) = (eval l x) && (eval l y)
eval l (Prop Or x y) = (eval l x) || (eval l y)
