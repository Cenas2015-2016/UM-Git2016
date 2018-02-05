module Ficha4Exe5 where


data Movimento = Credito Float | Debito Float
               deriving Show
data Data = D Int Int Int
          deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show


mov1 = Credito 10000
mov2 = Credito 10000
mov3 = Debito 280
mov4 = Debito 250
mov5 = Debito 270
mov6 = Credito 10000
mov7 = Credito 10000
mov8 = Credito 10000
mov9 = Debito 320
mov10 = Credito 10000
mov11 = Debito 50
mov12 = Debito 400
mov13 = Debito 120
mov14 = Debito 100
mov15 = Debito 500
mov16 = Credito 10000
mov17 = Credito 10000
mov18 = Debito 10 
mov19 = Debito 40
mov20 = Credito 3000

estratoBancario = Ext 9760 [((D 12 01 2017),"Cenas1",mov1),
                            ((D 12 01 2016),"Cenas2",mov2),
                            ((D 12 01 2015),"Cenas3",mov3),
                            ((D 12 01 2014),"Cenas4",mov4),
                            ((D 12 01 2013),"Cenas5",mov5),
                            ((D 12 01 2012),"Cenas6",mov6),
                            ((D 12 01 2011),"Cenas1",mov7),
                            ((D 12 01 2010),"Cenas8",mov8),
                            ((D 12 01 2019),"Cenas5",mov9),
                            ((D 12 01 2018),"Cenas10",mov10),
                            ((D 12 01 2009),"Cenas11",mov11),
                            ((D 12 01 2008),"Cenas12",mov12),
                            ((D 12 01 2007),"Cenas13",mov13),
                            ((D 12 01 2006),"Cenas14",mov14),
                            ((D 12 01 2005),"Cenas15",mov15),
                            ((D 12 01 2004),"Cenas16",mov16),
                            ((D 12 01 2003),"Cenas17",mov17),
                            ((D 12 01 2002),"Cenas18",mov18),
                            ((D 12 01 2001),"Cenas15",mov19),
                            ((D 12 01 2000),"Cenas20",mov20)]


lstDescricoes = ["Cenas1","Cenas5","Cenas10","Descricao","Cenas15"]
--a)
getValorDoMov::Movimento->Float
getValorDoMov (Credito x) = x
getValorDoMov (Debito x) = x

extValor::Extracto->Float->[Movimento]
extValor (Ext cred []) valor = []
extValor (Ext cred l@((dat,desc,mov):xs)) valor | ((getValorDoMov mov)>valor) = mov:(extValor (Ext cred xs) valor)
                                                | otherwise = extValor (Ext cred xs) valor
--b)
coleta::String->Extracto->[(Data,Movimento)]
coleta d l@(Ext val []) = []
coleta d l@(Ext val ((dat,desc,mov):xs)) | (desc==d) = (dat,mov):(coleta d (Ext val xs))
                                         | otherwise = coleta d (Ext val xs)


filtro::Extracto->[String]->[(Data,Movimento)]
filtro _ [] = []
filtro (Ext cred l@((dat,desc,mov):xs)) lstDes@(y:ys) = (coleta y (Ext cred l)) ++ (filtro (Ext cred xs) ys)


--c)
somaDeb::Extracto->Float
somaDeb (Ext saldo []) = 0
somaDeb (Ext saldo reg@((_,_,Debito y):xs)) = y + (somaDeb (Ext saldo xs))
somaDeb (Ext saldo (x:xs)) = somaDeb (Ext saldo xs)


somaCred::Extracto->Float
somaCred (Ext saldo []) = 0
somaCred (Ext saldo reg@((_,_,Credito y):xs)) = y + (somaCred (Ext saldo xs)) 
somaCred (Ext saldo (x:xs)) = somaDeb (Ext saldo xs)


credDeb::Extracto->(Float,Float)
credDeb extrList@(Ext _ []) = (0,0)
credDeb extrList@(Ext _ regist@((_,_,mov):xs)) = (somaCred extrList,somaDeb extrList)

--d)
saldo::Extracto->Float
saldo ext@(Ext saldo []) = 0
saldo ext@(Ext saldo extract) = (somaCred ext)-(somaDeb ext)