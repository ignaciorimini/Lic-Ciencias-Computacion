data Tree a = Empty | Node (Tree a) a (Tree a) deriving Show

-- Función que dado un valor x de tipo a, y un entero d, crea un árbol binario completo de altura d con el valor x en cada nodo.
completo :: a -> Int -> Tree a
completo x 0 = Empty
completo x d =
    let subTree = completo x (d - 1)
    in Node x subTree subTree


-- Función que dado un valor x de t ipo a y un entero n, crea un árbol binario balanceado de tamaño n, con el valor x en cada nodo.
balanceado :: a -> Int -> Tree a
balanceado x 0 = Empty
balancedo x n
    | n == 1 = Node x Empty Empty
    | even n = 
        let half = balanceado x (div n 2)
        in Node x half half
    | otherwise =
        let leftSubtree = balanceado x (div (n - 1) 2)
            rigthSubtree = balanceado x (div (n - 1) 2 + 1)
        in Node x leftSubtree rigthSubtree
