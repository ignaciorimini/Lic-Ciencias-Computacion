{-
insert :: Ord a => a -> Bin a -> Bin a
insert x Hoja = Nodo Hoja x Hoja
insert x (Nodo l b r) 
  | x <= b = Nodo (insert x l) b r
  | otherwise = Nodo l b (insert x r)

-- Poner los llamados recursivos al final de los AND es más eficiente.
isBST :: Ord a => BST a -> Bool
isBST Hoja = True
isBST (Nodo l a r) = 
  (isHoja l || a >= maxT l) &&
  (isHoja r || a < minT r) &&
  isBST l && isBST r

isHoja Hoja = True
isHoja _ = False

maxT (Nodo _ x Hoja) = x
maxT (Nodo _ x r) = maxT r

minT (Nodo Hoja x _) = x
minT (Nodo l x _) = minT l

___________________________________________________
P(t) : isBST t = true => isBST (insert x t) = true

Demostraremos por inducción estructural sobre BSTs:
- P(Hoja)
- Si vale P(l) y P(r) entonces vale P(Nodo l v r)

_____________________
- Caso Hoja: isBST Hoja = True => isBST (insert x Hoja) = True
= isBST (insert x Hoja)
<insert def 1>
= isBST (Nodo Hoja x Hoja)
<isBST def 2>
= (isHoja Hoja || x >= maxT Hoja) && (isHoja Hoja || x < minT Hoja) && isBST Hoja && isBST Hoja
<isHoja def 1, OR lazy, isBST def 1
= True && True && True && True
<AND def>
= True

____________________
- Caso (Nodo l a r): isBST (Nodo l a r) = True => isBST (insert x (Nodo l a r)) = True

(HIP) Si isBST (Nodo l a r) = True, entonces son True:
  1. isHoja l || a >= maxT l (HIP1)
  2. isHoja r || a < minT r (HIP2)
  3. isBST l (HIP3)
  4. isBST r (HIP4)

(HI1): Vale P(l): isBST l = True => isBST (insert x l) = True
(HI2): Vale P(r): isBST r = True => isBST (insert x r) = True

Queremos ver que: isBST (insert x (Nodo l a r)) = True
Análisis por casos para eliminar el insert:

i) x <= a: 
= isBST (insert x (Nodo l a r))
<insert def 2.1 por x <= a>
= isBST (Nodo (insert x l)) a r
<isBST def 2>
= (isHoja (insert x l) || a >= maxT (insert x l)) && (isHoja r || a < minT r) && isBST (insert x l) && isBST r
<HIP2, HIP3 + HI1, HIP4>
= (isHoja (insert x l) || a >= maxT (insert x l)) && True && True && True
<p && True = True>
= (isHoja (insert x l)) || (a >= maxT (insert x l))
<Lema1: insert x l devuelve algo que arranca con Node, entonces isHoja Node.. = False>
= False || a > maxT (insert x l)
<False || p = p>
= a > maxT (insert x l)

<Lema2: isHoja t = False => maxT (insert x t) = x || maxT (insert x t) = maxT t> Este lema se prueba por inducción estructural sobre árboles.

  <Por casos en l: i) si l es Hoja>
  = a > maxT (insert x Hoja) 
  <insert def 2>
  = a > maxT (Nodo Hoja x Hoja)
  <maxT def 1>
  = a > x
  <Se cumple por el caso que estamos analizando>
  = True

  <Por casos en l: ii) si l es (Nodo l1 a1 r1)>
  = a > maxT (insert x (Nodo l1 a1 r1))
  <HIP1: isHoja l false => a >= maxT l>

    <Lema2: si maxT (insert x t) = x>
    = a > x
    <Se cumple por el caso que estamos analizando>
    = True

    <Lema2: si maxT (insert x t) = maxT t>
    = a > maxT l
    <HIP1>
    = True
-}