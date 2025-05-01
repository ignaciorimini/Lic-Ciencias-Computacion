{-
Demostrar la siguiente propiedad sabiendo que xs es una lista de números naturales y que maxl y sum se definen:

maxl :: Nat a => [a] -> a
maxl [] = 0
maxl (x:xs) = max x (maxl xs)

sum :: Nat a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

Propiedad a demostrar: sum xs <= length xs * maxl xs

______________________________________________
Probaremos la propiedad utilizando inducción estructural sobre listas.
Dada una propiedad P sobre listas, para probar que vale P(xs) ∀xs :: [Nat a]
- probamos que vale P([])
- probamos que si vale P(xs), entonces también vale P(x:xs).

P(xs): sum xs <= length xs * maxl xs

Caso xs = []:
    = sum []                <sum 1era def>
    = 0                     <length []>
    = length []             
    = maxl []
    = length [] * maxl []
    <= length [] * maxl []

Caso inductivo: supongamos que vale P(xs): sum xs <= length xs * maxl xs (HI) y probemos P(x:xs): sum (x:xs) <= length (x:xs) * maxl (x:xs)
    = sum (x:xs)                <sum 2da def>
    = x + sum xs                <HI>
    <= x + length xs * maxl xs

Sabemos que 
length (x:xs) = 1 + length xs
maxl (x:xs) = max x (maxl xs)

Caso 1: x >= maxl xs
    sum (x:xs) <= length (x:xs) * maxl (x:xs)
    x + sum xs <= (1 + length xs) * x
    x + sum xs <= x + (length xs * x)
    sum xs <= length xs * x
Y esto último es cierto por hipótesis inductiva, pues x >= maxl xs

Caso 2: x < maxl xs
    sum (x:xs) <= length (x:xs) * maxl (x:xs)
    sum (x:xs) <= length (x:xs) * maxl xs
    x + sum xs <= (1 + length xs) * maxl xs
    x + sum xs <= maxl xs + (length xs * maxl xs)

Como por hipótesis inductiva 
    sum xs <= length xs * maxl xs
    sum xs + x <= x + (length xs * maxl xs)         <sumamos x ambos lados>
    sum xs + x <= maxl xs + (length xs * maxl xs)   <caso x < maxl xs>

Entonces sum (x:xs) <= length (x:xs) * maxl xs es siempre cierto.

-}