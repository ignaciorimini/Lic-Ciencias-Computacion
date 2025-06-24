import Seq
import ListSeq

-- Dado un natural n calcula la secuencia de los primeros n números de Fibonacci, con trabajo y profundidad W(n) = n y S(n) = lg(n). Utilizar scan.
fibSeq :: Seq s => Int -> s Int
fibSeq n =
    if n < 0 then singletonS 1 else
        let seq = tabulateS (\val -> (val, val)) n          -- <(0,0), (1,1), (2,2), (3,3), ...>
            (fibPairs, _) = scanS next (0,1) seq            -- (<(0,1),(1,1),(1,2),(2,3),(3,5),(5,8), ...>, ...)
        in mapS fst fibPairs                                -- <0,1,1,2,3,5,8...>
        where
            next (a,b) _ = (b, a + b)