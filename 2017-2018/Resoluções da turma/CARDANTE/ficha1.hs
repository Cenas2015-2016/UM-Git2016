module Ficha1 where 

type Ponto = (Float,Float)
type Lados = (Float,Float,Float)
type Hora = (Int,Int)
type Minutos = Int

-- |Função que calcula o perímetro de uma circunferência, dado o seu raio.
perimetro :: Float -> Float
perimetro raio = 2 * pi * raio

-- |Função que calcula a distância entre dois pontos.
dist :: Ponto -> Ponto -> Float
dist (x1,y1) (x2,y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- |Função que seleciona os primeiro e último elementos de uma lista. 
primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)

-- |Função que verifica se um inteiro m é múltiplo de n.
multiplo :: Int -> Int -> Bool
multiplo m n = (mod m n == 0)

-- |Função que devolve a lista de input ou remove o primeiro elemento se o comprimento da mesma for par ou ímpar, respetivamente.
truncaImpar :: [a] -> [a]
truncaImpar l | (mod (length l) 2 == 0) = l
              | otherwise = tail l

-- |Função que calcula o inteiro maior, dados 2.
max2 :: Int -> Int -> Int
max2 x y | (x >= y) = x
         | otherwise = y

-- |Função que calcula o inteiro maior, dados 3.
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)

-- |Função que calcula o número de raízes de um polinómio de 2º grau.
nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c | (value > 0) = 2 
              | (value == 0) = 1
              | otherwise = 0
              where value = (b^2 - 4*a*c)

-- |Função que calcula as raízes reais de um polinómio de 2º grau.
raizes :: Float -> Float -> Float -> [Float]
raizes a b c | (nRaizes a b c == 0) = []
             | (nRaizes a b c == 1) = [raiz0]
             | otherwise = [raiz1,raiz2]
             where value = sqrt (b^2 - 4*a*c)
                   raiz0 = (b*(-1))/(2*a)
                   raiz1 = (b*(-1) - value)/(2*a)
                   raiz2 = (b*(-1) + value)/(2*a)

-- |Função que gera um tuplo com os comprimentos dos lados de um triângulo.
ladosTriang :: Ponto -> Ponto -> Ponto -> Lados
ladosTriang a b c = (dist a b, dist a c, dist b c)

-- |Função que calcula o perímetro de um triângulo.
perimetroTriang :: Ponto -> Ponto -> Ponto -> Float
perimetroTriang a b c = sumSides (ladosTriang a b c)
                        where sumSides (x,y,z) = (x + y + z)

-- |Função que calcula o conjunto de pontos de um retângulo, dados dois, que pertencem a uma das diagonais.
pontosRectangulo :: Ponto -> Ponto -> [Ponto]
pontosRectangulo a@(x1,y1) b@(x2,y2) = [a,b,(x1,y2),(x2,y1)]

-- |Função que verifica se uma hora é válida.
verificaHora :: Hora -> Bool
verificaHora (h,m) = (h >= 0 && h <= 23) && (m >= 0 && m <= 59)

-- |Função que verifica se uma hora é ou não depois de outra.
compara :: Hora -> Hora -> Bool
compara (h1,m1) (h2,m2) = (h1 > h2) || (h1 == h2 && m1 > m2)

-- |Função que converte horas para minutos.
convertePMin :: Hora -> Minutos
convertePMin (h,m) = h * 60 + m

-- |Função que converte minutos para horas. 
convertePHora :: Minutos -> Hora
convertePHora minutos = (div minutos 60, mod minutos 60) 

-- |Função que calcula a diferença entre duas horas distintas (em minutos).
diferenca :: Hora -> Hora -> Minutos
diferenca a@(h1,m1) b@(h2,m2) | (h1 < h2) = convertePMin b - convertePMin a
                              | (h1 == h2) = convertePMin (0, aux m1 m2)
                              | (h2 == 0) = diferenca a (24,m2)
                              | otherwise = convertePMin a - convertePMin b
                              where aux m1 m2 | (m1 >= m2) = m1 - m2 
                                              | otherwise = m2 - m1 
 
-- |Função que adiciona x minutos a uma hora.
adicionaMin :: Hora -> Minutos -> Hora
adicionaMin (h,m) minutos = convertePHora(convertePMin (h,m) + minutos)