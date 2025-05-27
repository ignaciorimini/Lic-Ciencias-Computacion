{-
Dadas las siguientes definiciones:

data Tree a = Leaf a | Node a (Tree a) (Tree a)

flatten (Leaf x) = [x]
flatten (Node x lt rt) = flatten lt ++ [x] ++ flatten rt

mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node x lt rt) = Node (f x) (mapTree f lt) (mapTree f rt)

___________________________________________________
a) Demostrar que:
map f . flatten = flatten . mapTree f

P(t): (map f . flatten) t = (flatten . mapTree f) t

Demostraremos esta propiedad utilizando el principio de inducción estructural sobre Tree a:
- Vale P(Leaf x).
- Si vale P(l) y P(r) entonces vale P(Node x l r).

____________________
Caso base: Leaf x
= (map f . flatten) (Leaf x)
= map f (flatten (Leaf x))          <. def (composicion)>
= map f [x]                         <flatten def 1>
= [f x]                             <map def>

= (flatten . mapTree f) (Leaf x)
= flatten (mapTree f (Leaf x))      <. def (composicion)>
= flatten (Leaf (f x))              <mapTree def 1>
= [f x]                             <flatten def 1>

Como llegamos al mismo resultado, entonces se cumple el caso base:
(map f . flatten) (Leaf x) = [f x] = (flatten . mapTree f) (Leaf x)

____________________
Caso inductivo: (Node x l r)

Supongamos que se cumple el enunciado para l y r (HI), es decir, 
P(l): (map f . flatten) l = (flatten . mapTree f) l
P(r): (map f . flatten) r = (flatten . mapTree f) r
Y probemos el enunciado para P(Node x l r).

= (map f . flatten) (Node x l r)
= map f (flatten (Node x l r))                                  <. def (composicion)>
= map f (flatten l ++ [x] + flatten r)                          <flatten def 2>
= map f (flatten l) ++ map f [x] ++ map f (flatten r)           <distributiva por def map>
= flatten (mapTree f l) ++ map f [x] + flatten (mapTree f r)    <HI>
= flatten (mapTree f l) ++ [f x] ++ flatten (mapTree f r)       <map def>
= flatten (Node (f x) (mapTree f l) (mapTree f r))              <flatten def 2 inversa>
= flatten (mapTree f (Node x l r))                              <mapTree def 2 inversa>
= (flatten . mapTree f) (Node x l r)                            <. def (composicion)>

Entonces se cumple el enunciado para el caso inductivo.


-}