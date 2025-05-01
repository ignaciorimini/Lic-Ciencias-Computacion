data TreeView a t = EMPTY | ELT a | NODE t t
data Paren = Open | Close

showtS :: Seq a -> TreeView a (Seq a)

-- a) matchParen usando Divide & Conquer.
-- Si todos los paréntesis han sido emparejados, es decir, hay la misma cantidad de Open que de Close, entonces matchP s devuelve (0, 0) y matchParen evalúa a True.
matchParen :: Seq Paren -> Bool
matchParen s = matchP s == (0, 0)

-- La función matchP toma una secuencia de paréntesis y devuelve un par de enteros (i,j) tal que:
-- i es el número de paréntesis de cierre (Close) no emparejados.
-- j es el número de paréntesis de apertura (Open) no emparejados.
-- La idea es que la secuencia puede reducirse a )^i (^j eliminando todas las subsecuencias de la forma ().
matchP :: Seq Paren -> (Int, Int)
matchP s = case showtS s of
    EMPTY -> (0, 0)
    ELT P -> case p of
                Open -> (0, 1)
                Close -> (1, 0)
    NODE l r -> let ((il, jl), (ir, jr)) = matchP l || matchP r
                    matched = min jl ir
                in (il + ir - matched, jl + jr - matched)

-- Ejemplo: s = <Open, Close, Open, Close>
-- showtS s -> l = <Open, Close> | r = <Open, Close>
-- matchP <Open> = (0, 1)
-- matchP <Close> = (1, 0)
-- matched = min 1 1 = 1
-- matchP <Open, Close> = (0 + 1 - 1, 1 + 0 - 1) = (0, 0)

----------
-- Cálculo de costos de trabajo y profundidad.
-- Sea |s| = n, la longitud de la secuencia, entonces:

-- WmatchParen(n) = WmatchP(n)
-- WmatchP(n) = WshowtS(n) + c                                  si <showtS s> = EMPTY
--            = WshowtS(n) + c                                  si <showtS s> = ELT p
--            = WshowtS(n) + WmatchP(n/2) + WmatchP(n/2) + c    si <showtS s> = NODE l r
-- WmatchP(n) = O(1) + c                                        si <showtS s> = EMPTY
--            = O(1) + c                                        si <showtS s> = ELT p
--            = 2WmatchP(n/2) + O(1)                            si <showtS s> = NODE l r
-- Por teorema maestro, a = 2 | b = 2 | c = 0,
-- log2(2) = 1 > 0 -> WmatchP(n) = O(n^log2(2)) = O(n)
-- Luego, WmatchParen(n) = O(n)

-- SmatchParen(n) = SmatchP(n)
-- SmatchP(n) = Sshowt(n) + c                                   si <showtS s> = EMPTY
--            = Sshowt(n) + c                                   si <showtS s> = ELT p
--            = Sshowt(n) + max(SmatchP(n/2), SmatchP(n/2)) + c si <showtS s> = NODE l r
-- SmatchP(n) = O(1) + c
--            = O(1) + c
--            = SmatchP(n/2) + O(1)
-- Por teorema maestro, a = 1 | b = 2 | c = 0
-- log2(1) = 0 = c -> SmatchP(n) = O(n^0 lg n) = O(lg n)
-- Luego, SmatchParen(n) = O(lg n)


---------------------------------------------------------------
-- b) matchParen usando scan.
matchParen :: Seq Paren -> Bool
matchParen s =
    let (sec, accParen) = scan fScan 0 s
        balancesSec = filter (>= 0) sec
    in accParen == 0 && (length sec) == (length balancesSec)
    where
        fScan acc Open = acc + 1
        fScan acc Close = acc - 1
        

-- Se puede mejorar utilizando la función all <predicado> <sec>, que evalúa la función predicado en cada item de una secuencia y devuelve True si el predicado ha evaluado True en todos los items; False en caso contrario.
matchParen :: Seq Paren -> Bool
matchParen s =
    let (sec, accParen) = scan fScan 0 s
    in accParen == 0 && all (>=) sec
    where
        fScan acc Open = acc + 1
        fScan acc Close = acc - 1

----------
-- Cálculo de costos de trabajo y profundidad, suponiendo implementación de secuencias con arreglos persistentes.
-- Sea |s| = n, la longitud de la secuencia, entonces:

-- WmatchParen(n) = Wscan(fScan 0 n) + Wfilter(f n) + 2Wlength(n)
--                = O(n + W(fscan 0 s0) + ... W(fscan acc sn)) + O(W(>= s0 0) + ... W(>= sn 0)) + 2O(1)
--                = O(n + nc) + O(nc) + 2O(1)
--                = O(n(1 + c)) + O(nc) + O(1)
--                = O(n)

-- SmatchParen(n) = Sscan(fScan 0 n) + Sfilter((>= 0) n) + 2Slength(n)
--                = O(lg n . max fscan(0 si)) + O(lg n + max S((>= 0) si)) + 2O(1)
--                = O(lg n . c) + O(lg n + c) + O(1)
--                = O(lg n) + O(lg n) + O(1)
--                = O(lg n)