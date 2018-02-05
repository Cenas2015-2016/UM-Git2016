module Exercicio4F1 where


data Hora = H Int Int deriving (Show, Eq)

--a)
horaValidadDeriving::Hora->Bool
horaValidadDeriving (H h m) = (h>=0) && (h<24) && (m>=0) && (m<60)


--b)
verificaDeriving::Hora->Hora->Bool
verificaDeriving (H h1 m1) (H h2 m2) | (h1>h2) = True
                             | ((h1==h2) && (m1>m2)) = True
                             | otherwise = False

sequencialDeriving::Hora->Hora->Bool
sequencialDeriving h1 h2 | (horaValidadDeriving h1 && horaValidadDeriving h2) = verificaDeriving h1 h2
                         | otherwise = False


--c)
converteDeriving::Hora->Int
converteDeriving (H h m) = (h*60) + m

convertToMinutesDeriving::Hora->Int
convertToMinutesDeriving hora | (horaValidadDeriving hora) = converteDeriving hora
                              | otherwise = 0


--d)
converteToHoursDeriving::Int->Hora
converteToHoursDeriving min | (min<60) = (H 0 min)
                            | otherwise = (H (div min 60) (mod min 60))


--e)
calculaDeriving::Hora->Hora->Int
calculaDeriving h1 h2 = (convertToMinutesDeriving h1) - (convertToMinutesDeriving h2)


diferencaDeriving::Hora->Hora->Int
diferencaDeriving h1 h2 | ((horaValidadDeriving h1) && (horaValidadDeriving h2)) = calculaDeriving h1 h2
                        | otherwise = 0



--f)
addMinutesDeriving::Int->Hora->Hora
addMinutesDeriving min (H h m) | ((min+m)>59) = (H ((div min 60)+h) ((mod min 60)+m))
                               | otherwise = (H h (min+m))

adicionaDeriving::Int->Hora->Hora
adicionaDeriving min hora | (horaValidadDeriving hora) = addMinutesDeriving min hora
                          | otherwise = hora



