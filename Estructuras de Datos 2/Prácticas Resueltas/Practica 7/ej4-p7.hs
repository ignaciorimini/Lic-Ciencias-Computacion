import Seq
import ListSeq

-- Representan los caracteres '(' y ')' (paréntesis).
data Paren = Open | Close

-- matchParen: dada una secuencia de paréntesis, devuelve True si la secuencia contiene la misma cantidad de valores Open que Close y además cada prefijo de la secuencia no contiene menos valores Open que Close. Devuelve False en caso contrario.

----------------------------------------------
-- DIVIDE & CONQUER: Función implementada con Divide & Conquer. La función showT del TAD de secuencias puede ser de utilidad para dividir una secuencia en dos.
matchParen :: Seq s => s Paren -> Bool
matchParen s = matchP s == (0,0)

-- Si matchP s = (i,j), entonces s puede reducirse a )^i (^j eliminando todas las subsecuencias de la forma (). La función matchParen quedaría definida en términos de esta función como: matchParen s = matchP s == (0,0).
-- Es decir (i,j) significa que la secuencia es equivalente a:
--  - i paréntesis ) sin emparejar al principio
--  - j paréntesis ( sin emparejar al final, luego de cancelar todos los pares () posibles.
matchP :: Seq s => s Paren -> (Int, Int)
matchP s = case showtS s of
    EMPTY -> (0,0)
    ELT Open -> (0,1)
    ELT Close -> (1,0)
    NODE l r ->
        let (il, jl) = matchP l
            (ir, jr) = matchP r
            matcheados = min jl ir
            i' = il + (ir - matcheados)
            j' = jr + (jl - matcheados)
        in (i', j')


----------------------------------------------
-- USANDO SCAN: forma de utilizar Reduce/Scan que reemplaza a Divide & Conquer.
matchParen' :: Seq s => s Paren -> Bool
matchParen' seq =
    let (_, (i,j)) = scanS combine val (mapS base seq)
    in i == j
    where
        val = (0,0)
        base x = case x of
            Open -> (0,1)
            Close -> (1,0)
        combine (a,b) (c,d) = (a+c,b+d)


----------------------------------------------
-- Ejemplos.
seq1 = fromList [Open, Open, Open, Close, Close] :: ListSeq Paren
seq2 = fromList [Open, Open, Open, Close, Close, Close] :: ListSeq Paren
