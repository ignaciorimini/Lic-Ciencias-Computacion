class Cola t where
    vacia :: t a
    poner :: a -> t a -> t a
    sacar :: t a -> t a
    primero :: t a -> a
    esVacia :: t a -> Bool

instance Cola [] where
    vacia = []
    poner x xs = x:xs
    sacar xs = init xs
    primero = last
    esVacia xs = null xs

-- Envía los primeros n elementos al fondo de la cola.
-- Analogía: te rebotan en el boliche y volves a hacer la cola.
ciclar :: Cola t => Int -> t a -> t a
ciclar 0 cola = cola
ciclar n cola = ciclar (n-1) (poner (primero cola) (sacar cola))

-- Ejemplo de uso
main :: IO ()
main = do
    let cola = vacia :: [Int]
        cola1 = poner 1 cola
        cola2 = poner 2 cola1
        cola3 = poner 3 cola2
        primerElemento = primero cola3
        colaSinPrimerElemento = sacar cola3
        estaVacia = esVacia cola3
        colaCiclada = ciclar 2 cola3
    print primerElemento
    print colaSinPrimerElemento
    print estaVacia
    print colaCiclada