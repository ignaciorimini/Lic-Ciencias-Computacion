{-
Dado el siguiente tipo de datos:
data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

___________________________________________________
a) Dar el tipo y definir la función size que calcula la cantidad de elementos que contiene un (Arbol a).

size :: Arbol a -> Int
size (Hoja _) = 1
size (Nodo _ l r) = 1 + size l + size r

___________________________________________________
b) Demostrar la validez de la siguiente propiedad:
∀t ∈ (Arbol a). ∃k ∈ N. size t = 2k + 1

Demostraremos la propiedad utilizando el principio de inducción estructural sobre (Arbol a):
- P(Hoja)
- Si vale P(l) y P(r) entonces vale P(Nodo a l r)

- Caso Hoja.
= size (Hoja v)
= 1             <size def 1>
= 2*0 + 1       <aritmetica>
= 2k + 1 con k = 0
Entonces vale el caso base, con k = 0.

- Caso Nodo.
Suponemos que se cumple P(l): size l = 2n + 1 y P(r): size r = 2m + 1 (HI) y probamos para P(Nodo v l r)
= size (Nodo v l r)
= 1 + size l + size r       <size def 2>
= 1 + (2n + 1) + (2m + 1)   <HI en l y r>
= (2n + 2m + 2) + 1         <asociativa>
= 2(n+m+2) + 1              <distributiva>
=> k = n+m+2
Entonces se cumple el caso inductivo, con k = n + m + 2.

___________________________________________________
c) Dar el tipo y definir la funcion mirror que dado un árbol devuelve su árbol espejo.

mirror :: Arbol a -> Arbol a
mirror (Hoja v) = (Hoja v)
mirror (Nodo v l r) = (Nodo v (mirror r) (mirror l))

___________________________________________________
d) Demostrar la validez de la siguiente propiedad:
mirror . mirror = id

Demostraremos la propiedad utilizando el principio de inducción estructural sobre (Arbol a):
- P(Hoja)
- Si vale P(l) y P(r) entonces vale P(Nodo v l r)

- Caso (Hoja v)
= mirror . mirror (Hoja v)
= mirror (mirror (Hoja v))    <. def (composicion)>
= mirror (Hoja v)             <mirror def 1>
= (Hoja v)
Entonces se cumple el caso base.

- Caso (Nodo v l r).
Suponemos que se cumple P(l): mirror . mirror l = l y P(r): mirror . mirror r = r (HI) y probamos para P(Nodo v l r)
= mirror . mirror (Nodo v l r)
= mirror (mirror (Nodo v l r))                    <. def (composicion)>
= mirror (Nodo v (mirror l) (mirror r))           <mirror def 2>
= Nodo v (mirror (mirror l)) (mirror (mirror r))  <mirror def 2>
= Nodo v l r                                      <HI>
Entonces se cumple el caso inductivo.

___________________________________________________
e) Considerando las siguientes funciones:
hojas :: Arbol a -> Int
hojas (Hoja x) = 1
hojas (Nodo x t1 t2) = hojas t1 + hojas t2

altura :: Arbol a -> Int
altura (Hoja x) = 1
altura (Nodo x t1 t2) = 1 + (max (altura t1) (altura t2))

Demostrar que para todo árbol finito t se cumple que:
hojas t < 2^(altura t)

Demostraremos la propiedad usando el principio de inducción estructural sobre (Arbol a):
- P(Hoja)
- Si vale P(l) y P(r) entonces vale P(Nodo v l r)

- Caso Hoja.
= hojas (Hoja v)
= 1                   <hojas def 1>
< 2                   <aritmetica>
= 2^1                 <aritmetica>
= 2^(altura (Hoja v)) <altura def 1>
Entonces se cumple el caso base.

- Caso (Nodo v l r).
Suponemos que se cumplen P(l): hojas l < 2^(altura l) y P(r): hojas r < 2^(altura r) y probamos P(Nodo v l r).
= hojas (Nodo v l r)
= hojas l + hojas r                   <hojas def 2>
< 2^(altura l) + 2^(altura r)         <HI>
<= 2^(1 + max (altura l) (altura r))  <Lema1: 2^a + 2^b <= 2^{1 + max a b}
= 2^(altura (Nodo v l r))             <altura def 2 (inverso)>
Entonces se cumple el caso inductivo.

_________________
Prueba de Lema 1: 2^a + 2^b <= 2^{1 + max a b}.

Probaremos este lema utilizando principio de inducción de naturales sobre a.

Caso base: a = 0.
= 2^0 + 2^b
= 1 + 2^b
= 2^{1+b}
= 2^{1 + max a b}

Caso inductivo. Suponemos que se cumple P(x) para x < a y probamos para P(a).
= 2^a + 2^b
= 2 * 2^{a - 1} + 2 * 2^{b-1}
= 2 * (2^{a-1} + 2^{b-1})
= 2 * 2^{1 + max (a - 1) (b - 1)} <HI>
= 2^{1 + 1 + max (a - 1) (b - 1)}
= 2^{1 + max a b}



-}