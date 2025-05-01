{-
Dadas las siguientes definiciones:

data Tree a = Leaf a | Node a (Tree a) (Tree a)

-- Esta función toma un árbol y lo convierte en una lista de sus elementos in-order.
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node x lt rt) = flatten lt ++ [x] ++ flatten rt

-- Esta función aplica una función f a cada elemento de un árbol.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node x lt rt) = Node (f x) (mapTree f lt) (mapTree f rt)

-------------
Demostrar que map f . flatten = flatten . mapTree f

P(t): map f (flatten t) = flatten (mapTree f t)

Demostraremos esta propiedad por inducción estructural sobre Tree.
Dada una propiedad P sobre elementos de tipo Tree a, para probar que vale P(t) para todo t :: Tree:
- probamos que vale P(Leaf a)
- probamos que si vale P(izq) y P(der), entonces también vale P(Node a izq der).

-----
Caso t = Leaf a
    = map f (flatten (Leaf x))          <flatten 1era def>
    = map f [x]                         <map def>
    = [f x]

    Luego
    = flatten (mapTree f (Leaf x))      <mapTree 1era def>
    = flatten (Leaf f x)                <flatten 1era def>
    = [f x]

Y como se cumple la igualdad entonces la propiedad vale para el caso base.

-----
Caso t = Node x izq der: supongamos que valen las siguientes propiedades:
HI1) P(izq): map f (flatten izq) = flatten (mapTree f izq)
HI2) P(der): map f (flatten der) = flatten (mapTree f der)

    = map f (flatten (Node x izq der))              <flatten 2da def>
    = map f (flatten izq ++ [x] ++ flatten der)     <map def>
    = map f (flatten izq) ++ [f x] ++ map f (flatten der)
    
    Luego
    = flatten (mapTree f (Node x izq der))                  <mapTree 2da def>
    = flatten (Node (f x) (mapTree f izq) (mapTree f der))  <flatten 2da def>
    = flatten (mapTree f izq) ++ [f x] ++ flatten (mapTree f der)   <HI1>
    = map f (flatten izq) ++ [f x] ++ flatten (mapTree f der)       <HI2>
    = map f (flatten izq) ++ [f x] ++ map f (flatten der)

Y como las igualdades coinciden, entonces la propiedad vale para el caso inductivo.
Luego, como vale para el caso base y para el caso inductivo, entonces la propiedad es válida para todo t :: Tree.



-}
