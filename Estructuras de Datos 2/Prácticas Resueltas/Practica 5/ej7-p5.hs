{-
7) Demostrar que sum xs <= length xs * maxl xs, sabiendo que xs es una lista de números naturales y que maxl y sum se definen:

maxl [] = 0
maxl (x:xs) = max x (maxl xs)

sum [] = 0
sum (x:xs) = x + sum xs

___________________________________________________
Demostraremos la propiedad utilizando el principio de inducción estructural sobre listas:
- P([])
- Si vale P(xs) => vale P(x:xs)

P(xs): sum xs <= length xs * maxl xs

---------------------
- Caso base: xs = []
= sum []
= 0                     <sum def 1>
= 0 * 0                 <aritmetica>
= length [] * 0         <length def 1>
= length [] * maxl xs   <maxl def 1>

Entonces se cumple que sum [] <= length [] * maxl [].

---------------------
- Caso inductivo: xs.
Suponemos que se cumple P(xs): sum xs <= length xs * maxl xs (HI) y probaremos P(x:xs).
= sum (x:xs)
= x + sum xs                                            <sum def 2>
<= x + (length xs * maxl xs)                            <HI>
<= (max x (maxl xs)) + length xs * (max x (maxl xs))    <Lema 1: para a, b naturales a <= max a b>
= (1 + length xs) * (max x (maxl xs))                   <factor común>
= length (x:xs) * (max x (maxl xs))                     <length def 2>
= length (x:xs) * maxl (x:xs)                           <maxl def 2 (inversa)>

Entonces se cumple sum (x:xs) <= length (x:xs) * maxl (x:xs)
-}