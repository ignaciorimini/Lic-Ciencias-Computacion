type Interval = (Int, Int)
data ITree = E | N ITree Interval ITree deriving Show

-- Dado un árbol no vacío, devuelve el extremo derecho del intervalo de más a la derecha del árbol.
right :: ITree -> Int
right (N E inter E) = snd inter
right (N izq inter der) = right der


-- Dado un valor de tipo ITree chequea si éste es un árbol de intervalo.
-- Esta función puede definirse con trabajo en O(2^h), donde h es la altura del arbol.
checkIT :: ITree -> Bool
checkIT E = True
checkIT (N izq (a,b) der) =
    let izqValido = (disjointsIntervals (getInterval izq) (a,b)) && checkIT izq
        derValido = (disjointsIntervals (getInterval der) (a,b)) && checkIT der
    in (validInterval (a,b)) && izqValido && derValido

validInterval :: Interval -> Bool
validInterval (a,b) = a <= b

disjointsIntervals :: Interval -> Interval -> Bool
disjointsIntervals (x1,y1) (x2,y2) =
    let invarianteB = y1 <= (x2 - 1)
        invarianteC = (y2 + 1) < x1
    in invarianteB && invarianteC

getInterval :: ITree -> Interval
getInterval (N _ par _) = par


-- Dado un árbol de intervalo t, devuelve el intervalo que está más a la derecha en t (es decir, el que tiene su primer componente mayor a la del resto), y el árbol t sin este elemento.
splitMax :: ITree -> (Interval, ITree)
splitMax (N E (a,b) E) = ((a,b), E)
splitMax (N izq (a,b) der) = 
    let inter = fst (splitMax der)
        tree = N izq (a,b) (snd (splitMax der))
    in (inter, tree)


-- -- Función tal que si N l i r es un árbol de intervalo, merge l r devuelve un árbol de intervalo con los elementos de l y r.
-- -- Ayuda: usar función del inciso anterior.
-- merge :: ITree -> ITree -> ITree
-- merge E arbol = arbol
-- merge arbol E = arbol
-- merge arbol1 arbol2 =
--     let ((x1,y1), arbolSplit1) = splitMax arbol1
--         ((x2,y2), arbolSplit2) = splitMax arbol2
--         ... | x2 >= x1 && y2 <= y1  = 
--     in merge arbolSplit1 arbolSplit2


