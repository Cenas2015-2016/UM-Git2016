--Exercicio 1
toDigit::Int->String
toDigit [] = []
toDigit x = (head(reverse(show x))):(toDigit (read(tail(reverse(show x)))))