module Exercicio3F1 where

type Hora = (Int,Int)

--a)
horaValida::Hora->Bool
horaValida (h,m) = (h>=0) && (h<24) && (m>=0) && (m<60)

--b)
verifica::Hora->Hora->Bool
verifica (h1,m1) (h2,m2) | (h1>h2) = True
                         | ((h1==h2) && (m1>m2)) = True
                         | otherwise = False

sequencial::Hora->Hora->Bool
sequencial h1 h2 | (horaValida h1 && horaValida h2) = verifica h1 h2
                 | otherwise = False


--c)
converte::Hora->Int
converte (h,m) = (h*60) + m

convertToMinutes::Hora->Int
convertToMinutes hora | (horaValida hora) = converte hora
                      | otherwise = 0


--d)
converteToHours::Int->Hora
converteToHours min | (min<60) = (0,min)
                    | otherwise = (div min 60,mod min 60)


--e)
calcula::Hora->Hora->Int
calcula h1 h2 = (convertToMinutes h1) - (convertToMinutes h2)


diferenca::Hora->Hora->Int
diferenca h1 h2 | ((horaValida h1) && (horaValida h2)) = calcula h1 h2
                | otherwise = 0



--f)
addMinutes::Int->Hora->Hora
addMinutes min (h,m) | ((min+m)>59) = ((div min 60)+h, (mod min 60)+m)
                     | otherwise = (h,min+m)

adiciona::Int->Hora->Hora
adiciona min hora | (horaValida hora) = addMinutes min hora
                 | otherwise = hora



