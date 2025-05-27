{-
Dadas las siguientes definiciones:
data AGTree a = Node a [AGTree a]
ponerProfs n (Node x xs) = Node n (map (ponerProfs (n+1)) xs)

___________________________________________________
a) Definir una función alturaAGT que calcule la altura de un AGTree.

alturaAGT :: AGTree a -> Int
alturaAGT (Node _ []) = 1
alturaAGT (Node x xs) = 1 + maximum (map alturaAGT xs) 

___________________________________________________
b) Definir una función maxAGT que dado un AGTree de enteros devuelva su mayor elemento.

maxAGT :: AGTree Int -> Int
maxAGT (Node x []) = x
maxAGT (Node x xs) = max x (maximum (map maxAGT xs))

___________________________________________________
c) Demostrar que alturaAGT = maxAGT . ponerProfs 1

P(t): alturaAGT t = (maxAGT . ponerProfs 1) t

Demostraremos la propiedad utilizando el principio de inducción estructural sobre AGTree:
- Vale P(Node x []).
- Si vale P(xs) entonces vale P(Node x xs).

____________________
Caso base: Node x []
= alturaAGT (Node x [])
= 1                                             <alturaAGT def 1>

= (maxAGT . ponerProfs 1) (Node x [])
= maxAGT (ponerProfs 1 (Node x []))             <. def (composicion)>
= maxAGT (Node 1 (map (ponerProfs 2) []))       <ponerProfs def>
= maxAGT (Node 1 [])                            <map def 1>
= 1                                             <maxAGT def 1>

Como en ambos casos llegamos a lo mismo, entonces vale el caso base:
-> alturaAGT t = 1 = (maxAGT . ponerProfs 1) t

____________________
Caso inductivo: Node x xs

Supongamos que se cumple el enunciado para cada árbol s en xs (HI):
P(s): alturaAGT s = (maxAGT . ponerProfs 1) s
Y probemos la propiedad para (Node x xs)

= alturaAGT (Node x xs)
= 1 + maximum (map alturaAGT xs)                            <alturaAGT def 2>
= 1 + maximum (map (\s -> maxAGT (ponerProfs 1 s)) xs)      <HI>
= 1 + maximum (map maxAGT (map (ponerProfs 2) xs))          <ponerProfs 1 (Node x xs) = Node 1 (map (ponerProfs 2) xs)
= max 1 (maximum (map maxAGT (map (ponerProfs2) xs)))       <max a b = 1 + maximum [a-1, b-1]>
= maxAGT (Node 1 (map (ponerProfs 2) xs))                   <maxAGT def 2 inversa>
= maxAGT (ponerProfs 1 (Node x xs))                         <ponerProfs def inversa>

Entonces se cumple el caso inductivo.

-}