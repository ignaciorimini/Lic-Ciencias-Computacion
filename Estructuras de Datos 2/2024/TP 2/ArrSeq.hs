module ArrSeq where

import qualified Arr as A
import Arr ((!))
import Seq
import Par

instance Seq A.Arr where
    -- Función que no recibe argumentos y devuelve la secuencia vacía.
    emptyS = A.empty


    -- Función que dado un elemento de tipo a, devuelve una secuencia de tamaño 1 con solo ese elemento.
    -- s = singletonS 1 :: A.Arr Int -> <1>
    singletonS x = A.tabulate (const x) 1


    -- Función que dada una secuencia, devuelve el tamaño de la misma.
    lengthS s = A.length s


    -- Función que dada una secuencia y un entero, devuelve el elemento de la secuencia cuyo índice es el entero dado.
    nthS s i = s ! i


    -- Función que recibe dos argumentos: una función y un entero. Devuelve una secuencia de tamaño especificado por el segundo argumento, donde los elementos de la misma se obtienen de aplicar la función argumento a cada índice de la secuencia.
    -- s = tabulateS (+ 1) 3 :: A.Arr Int -> <1,2,3>
    tabulateS f n = A.tabulate f n


    -- Función que recibe una función f y una secuencia, y aplica la función f a cada elemento de la secuencia dada.
    mapS f s = A.tabulate (\i -> f (s ! i)) (A.length s)


    -- Función que dadas dos secuencias s y t, las concatena poniendo s primero y luego t.
    appendS s t = 
        let sizeS = A.length s
            sizeT = A.length t
            funcionDeUnion i = 
                if i < sizeS 
                    then s ! i 
                    else t ! (i - sizeS)
        in A.tabulate funcionDeUnion (sizeS + sizeT)


    -- Función que recibe una función predicado "p" y una secuencia "s", y devuelve una subsecuencia de "s" con únicamente los elementos de "s" que cumplen con el predicado "p". Es decir, los elementos tal que <p s> devuelve True.
    filterS p s
        | size == 0 = A.empty
        | size == 1 = if p (s ! 0) then s else A.empty
        | otherwise = appendS izqFiltered derFiltered
        where
            size = A.length s
            mitad = div size 2
            (izq, der) = (A.subArray 0 mitad s, A.subArray mitad (size - mitad) s)
            (izqFiltered, derFiltered) = (filterS p izq ||| filterS p der)


    -- Función que recibe una secuencia "s" y un entero "n", y devuelve una subsecuencia de "s" que tiene los mismos elementos que "s" sin los últimos (lengthS s) - n elementos.
    takeS s n = 
        let size = A.length s
        in if n >= size
            then s
            else A.subArray 0 n s


    -- Función que recibe una secuencia "s" y un entero "n", y devuelve una subsecuencia de "s" que tiene los mismos elementos que "s" sin los primeros "n" elementos.
    dropS s n = 
        let size = A.length s
        in if n >= size
            then A.empty
            else A.subArray n (size - n) s
    

    -- Función que dada una secuencia, devuelve la vista en forma de árbol de la misma.
    showtS s
        | size == 0 = EMPTY
        | size == 1 = ELT (s ! 0)
        | otherwise = NODE (takeS s (div size 2)) (dropS s (div size 2))
        where
            size = A.length s
    

    -- Función que dada una secuencia, devuelve la vista en forma de lista de la misma.
    showlS s
        | size == 0 = NIL
        | otherwise = CONS (s ! 0) (dropS s 1)
        where
            size = A.length s


    -- Función que dada una secuencia de secuencias, une todas las secuencias internas en una sola.
    joinS s = A.flatten s


    -- Función que dada una lista, devuelve una secuencia con los mismos elementos de la lista.
    fromList l = A.fromList l


    -- Función que reduce la secuencia a un solo valor usando la función dada y un valor inicial.
    reduceS f acc s
        | size == 0 = acc
        | size == 1 = f acc (s ! 0)
        | otherwise = reduceS f acc (contract s f)
        where
            size = A.length s
            contract s f
                | size <= 1 = s
                | otherwise = 
                    let mitad = div (size + 1) 2
                    in A.tabulate (\i -> if 2 * i + 1 < size
                                         then f (s ! (2 * i)) (s ! (2 * i + 1))
                                         else s ! (2 * i)) mitad


    -- Función que reduce la secuencia a un solo valor usando la función dada y un valor inicial. Retorna un par de dos elementos, donde el primero es la secuencia que muestra la aplicación parcial de la función para obtener la reducción, y el segundo valor es el resultado de la reducción en sí.
    scanS f acc s
        | lengthS s == 0 = (emptyS, acc)
        | lengthS s == 1 = (singletonS (f acc (s ! 0)), f acc (s ! 0))
        | otherwise = (expand s s', res)
            where
                (s', res) = scanS f acc (contract s)

                expand s1 s2 = tabulateS g (lengthS s)
                    where
                        g i
                            | i == 0 = acc
                            | even i = s2 ! (div i 2)
                            | otherwise = 
                                if A.length s2 == 1
                                    then s1 ! 0
                                    else f (s2 ! (div i 2)) (s1 ! (i - 1))

                contract seq
                    | lengthS seq <= 1 = seq
                    | otherwise = tabulateS h mitad
                        where 
                            size = lengthS seq
                            mitad = div (size + 1) 2
                            h i = if ((2 * i) + 1) < size
                                    then f (seq ! (2 * i)) (seq ! ((2*i) + 1))
                                    else seq ! (2 * i)
                
                
                

