{-
Dado el siguiente tipo de datos:
data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

______________________________________________
a) Dar el tipo y definir la función size que calcula la cantidad de elementos que contiene un (Arbol a).

size :: Arbol a -> Int
size Hoja _ = 1
size (Nodo _ izq der) = 1 + size izq + size der

______________________________________________
b) Demostrar la validez de la siguiente propiedad:
∀t :: (Arbol a), ∃k :: N -> size t = 2k + 1

Demostraremos esta propiedad utilizando inducción estructural de Arbol a.
Dada una propiedad P sobre elementos de tipo Arbol, para probar que ∀t :: Arbol vale P(t):
- probamos P(Hoja)
- probamos que si P(izq) y P(der) valen, entonces vale P(Nodo val izq der).

Caso t = Hoja n:
    = size Hoja n           <size 1era def>
    = 1
    = 2.0 + 1

Luego, existe k = 0 tq size Hoja = 2k + 1.


Caso t = Nodo val izq der: suponemos que valen 
HI1) P(izq): size izq = 2p + 1
HI2) P(der): size der = 2h + 1
Y mostramos la propiedad para P(Nodo val izq der):
    = size (Nodo val izq der)           <size 2da def>
    = 1 + size izq + size der           <HI1>
    = 1 + 2p + 1 + size der             <HI2>
    = 1 + 2p + 1 + 2h + 1
    = (2p + 2h + 2) + 1

Luego, existe un k = 2p + 2h + 2, donde p es el natural tal que size izq = 2p + 1, y h es el natural tal que size der = 2h + 1, de forma tal que size (Nodo val izq der) = 2k + 1.

Luego, esta propiedad vale para todo t :: Arbol.

______________________________________________
c) Dar el tipo y definir la función mirror que dado un árbol devuelve su espejo.

mirror :: Arbol a -> Arbol a
mirror Hoja val = Hoja val
mirror (Nodo val izq der) = (Nodo val der izq)

______________________________________________
d) Demostrar la validez de la siguiente propiedad: mirror . mirror = id

P(t): mirror (mirror t) = t

Demostraremos esta propiedad utilizando nuevamente la inducción estructural sobre elementos de tipo Arbol.
Dada una propiedad P sobre elementos de tipo Arbol, para probar que ∀t :: Arbol vale P(t):
- probamos que vale P(Hoja)
- probamos que si P(izq) y P(der) valen, P(Nodo val izq der) también vale.

Caso t = Hoja n:
    = mirror (mirror Hoja n)            <mirror 1era def>
    = mirror Hoja n                     <mirror 1era def>
    = Hoja n

Caso t = Nodo val izq der: suponemos que valen:
HI1) P(izq): mirror (mirror izq) = izq
HI2) P(der): mirror (mirror der) = der
Y probamos la propiedad para P(Nodo val izq der):
    = mirror (mirror Nodo val izq der)  <mirror 2da def>
    = mirror Nodo val der izq           <mirror 2da def>
    = Nodo val izq der

Y luego, la propiedad P vale para todos los elementos t :: Arbol.

______________________________________________
e) Considerando las siguientes funciones:

hojas :: Arbol a -> Int
hojas (Hoja x) = 1
hojas (Nodo x t1 t2) = hojas t1 + hojas t2

altura :: Arbol a -> Int
altura (Hoja x) = 1
altura (Nodo x t1 t2) = 1 + max (altura t1) (altura t2)

Demostrar que para todo árbol finito t se cumple que hojas t < 2^(altura t)

P(t): hojas t < 2^(altura t) ∀t :: Arbol.

Probaremos esta propiedad nuevamente utilizando inducción estructural sobre elementos de tipo Arbol.
Dada una propiedad P sobre elementos de tipo Arbol, para probar que ∀t :: Arbol vale P(t):
- probamos que vale P(Hoja)
- probamos que si P(izq) y P(der) valen, P(Nodo val izq der) también vale.

Caso t = Hoja n
    = hojas Hoja n          <hojas 1era def>
    = 1
    < 2
    = 2^1                   <altura 1era def>
    = 2^(altura Hoja n)

Caso t = Nodo val izq der: suponemos que valen:
HI1) hojas izq < 2^(altura izq)
HI2) hojas der < 2^(altura der)
Y probamos la propiedad para P(Nodo val izq der):
    = hojas Nodo val izq der            <hojas 2da def>
    = hojas izq + hojas der             <HI1>
    < 2^(altura izq) + hojas der        <HI2>
    < 2^(altura izq) + 2^(altura der)

------------
Sea h1 = altura izq y h2 = altura der,
Queremos llegar a que 
hojas izq + hojas der < 2^altura(t) = 2^(1 + max h1 h2)
Sabemos que 2^(1 + max(h1, h2)) = 2 2^max(h1, h2)
Ahora veamos dos casos:

---
Caso h1 >= h2:
    altura (Nodo x t1 t2) = 1 + max h1 h2       <altura 2da def>
    altura (Nodo x t1 t2) = 1 + h1

Por hipótesis inductiva sabemos que hojas izq < 2^h1, y como h1 >= h2, también tenemos que hojas der < 2^h1. Luego,
    = hojas izq + hojas der
    < 2^h1 + 2^h1
    = 2 2^h1
    = 2^(1 + h1)
    = 2^(1 + max h1 h2)
    = 2^(1 + max (altura izq) (altura der))

Y se cumple la propiedad en este caso.

---
Caso h1 < h2:
    altura (Nodo x t1 t2) = 1 + max h1 h2
    altura (Nodo x t1 t2) = 1 + h2

Por hipótesis inductiva: hojas der < 2^h2, y como h1 < h2, también tenemos que hojas izq < 2^h2. Luego,
    = hojas izq + hojas der
    < 2^h2 + 2^h2
    = 2 2^h2
    = 2^(1 + h2)
    = 2^(1 + max h1 h2)
    = 2^(1 + max (altura izq) (altura der))

Y finalmente, se cumple para ambos casos. Luego, por inducción estructural sobre Arbol, hemos comprobado que la propiedad P se cumple para todos los elementos de tipo Arbol.
-}