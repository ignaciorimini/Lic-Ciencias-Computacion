fibSeq :: Nat -> Seq Nat
fibSeq n = 
    let (seqPares, lastPar) = scan (\(a, b) _ -> (b, a + b)) (0, 1) (tabulateS id n)
    in mapS fst seqPares

-- Para analizar los costos de fibSeq, suponemos que la implementación del TAD de secuencias está hecha con arreglos persistentes (nos valemos de los costos de las funciones para arreglos persistentes especificados en el glosario).
-- W(n) = Wscan(f e (tabulate id n))
--      = Wscan(f e n) + Wtabulate(id n)
--      = Wscan(f e n) + O(Wid(0) + Wid(1) + ... + Wid(n))
--      = Wscan(f e n) + O(1 + 1 + ... 1)
--      = Wscan(f e n) + O(n)
--      = O(|s| + W(f (0, 1) _ ) + W(f (a, b) _) + ...) + O(n)
--      = O(n + c + c + ... c) + O(n)
--      = O(n + cn) + O(n)
--      = O(n(1 + c)) + O(n)
--      = Wmap(f n) + 2O(n)
--      = O(W(f s0) + W(f s1) + ... W(f sn)) + O(n)
--      = O(1 + 1 + ... + 1) + O(n)
--      = O(n) + O(n)
--      = O(n)

-- S(n) = Sscan(f (0, 1) (tabulate id n))
--      = Sscanf(f e n) + Stabulate(id n)
--      = Sscan(f e n) + O(max S(id 0) S(id 1) S(id 2) ...)
--      = Sscan(f e n) + O(1)
--      = O(lg |s| . max S(f (0,1) i) S(f (a, b) i) S(f (a,b) i) ...)
--      = O(lg n . c)
--      = Smap(f n) + O(lg n)
--      = O(max S(fst s0) S(fst s1) S(fst s2) ...) + O(lg n)
--      = O(1) + O(lg n)
--      = O(lg n)

