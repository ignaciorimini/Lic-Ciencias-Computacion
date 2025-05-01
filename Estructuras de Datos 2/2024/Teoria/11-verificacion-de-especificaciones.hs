{-
RAZONANDO CON PROGRAMAS

________________________________________________
VERIFICACIÓN DE LA ESPECIFICACIÓN DE UN PROGRAMA
Dada una implementación TAD, ¿cómo sabemos que es correcta? 
Debemos ver que implementa las operaciones y que estas operaciones verifican la especificacion.

Dada una implementación en Haskell, el sistema de tipos asegura que los tipos de las operaciones son correctos. Pero la verificación de la especificación la debe hacer el programador.

Lo que debemos ver es como verificar la especificación.

________________________________________________
RAZONAMIENTO ECUACIONAL
Haskell permite razonar ecuacionalmente acerca de las definiciones en forma similar al álgebra.

Si tenemos la siguiente función:
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

Probaremos que reverse [x] = [x]:
= reverse [x]       <def listas>
= reverse (x : [])  <reverse 2da def>
= reverse [] ++ [x] <reverse 1era def>
= [] ++ [x]         <++ 1era def>
= [x]

Notar que en las ecuaciones usamos = y no ==.

________________________________________________
PATRONES DISJUNTOS
Consideremos la siguiente función:
esCero :: Int -> Bool
esCero 0 = True
esCero n = False

La segunda ecuación es aplicable sólo si n no es cero. Por lo tanto, es más fácil razonar ecuacionalmente si los patrones son disjuntos:

esCero' :: Int -> Bool
esCero 0 = True
esCero n | n != 0 = False

Teniendo patrones disjuntos no hace falta tener en cuenta el orden de las ecuaciones.

________________________________________________
EXTENSIONALIDAD
Dadas dos funciones f, g :: A -> B
¿Cómo probamos que f = g?

Si tomamos la visión de caja negra sobre las funciones, solo podemos evaluar el comportamiento de una función viendo cómo se comporta al aplicarle argumentos.

Principio de extensionalidad: f = g sii Para todo x perteneciente a A, f(x) = g(x)

Obs: por extensionalidad los algoritmos QUICKSORT, INSERTION SORT, MERGESORT son iguales, pues para una misma entrada (una lista) devuelven la misma salida (la lista ordenada). 

Es decir, en el enfoque de extensionalidad para verificar igualdad de funciones no se tiene en cuenta la complejidad algorítmica.

________________________________________________
ANÁLISIS POR CASOS
Podemos hacer análisis por casos para probar propiedades:
not :: Bool -> Bool
not False = True
not True = False

Probaremos not (not x) = x, por casos de x:

Caso x = False
= not (not False)   <not 1era def>
= not True          <not 2da def>
= False

Caso x = True
= not (not True)    <not 2da def>
= not False         <not 1era def>
= True

________________________________________________
RAZONANDO CON PROGRAMAS RECURSIVOS - INDUCCIÓN
Para poder probar propiedades acerca de programas recursivos usualmente necesitamos usar inducción.

La inducción nos da una forma de escribir una prueba infinita de una manera finita.

Si quisiesemos probar una propiedad P para todo número natural, por ejemplo, P(n) = n es par o impar.
Con un papel infinito y con tiempo infinito, podríamos probar P(0), P(1), P(2) y así sucesivamente.

La inducción es una forma de probar que con papel infinito y tiempo infinito, podríamos completar la prueba.

________________________________________________
INDUCCIÓN SOBRE N: Primera forma
Definición. Para probar P(n) para todo n Natural, probamos P(0) y probamos que para cualquier m, si P(m) vale, entonces P(m+1) también.

- La prueba P(0) la llamamos caso base.
- La prueba de que P(m) -> P(m+1) es el paso inductivo.
- El suponer P(m) verdadero es la hipótesis de inducción.

________________________________________________
INDUCCIÓN SOBRE N: Segunda forma
Definición. Para probar P(n) para todo n Natural, probamos que para cualquier m, si vale P(i) ∀i < m, entonces vale P(m).

- No hay caso base.
- Suponemos verdadero P(i) ∀i < m (hipótesis de inducción).
- Esta forma se llama inducción fuerte. Pero en realidad, es tan completa o fuerte como la anterior inducción.

________________________________________________
INDUCCIÓN SOBRE OTROS CONJUNTOS
Podemos usar la inducción sobre los naturales para obtener inducción sobre otros conjuntos.

Por ejemplo, podemos hacer inducción sobre la altura de un árbol o la longitud de una lista.

En general, dada una función f : A -> N, y una propiedad P sobre elementos de A, podemos definir:
Q(n) = si vale f(a) = n => vale P(a) (para todo a de A).

Y es así como transformamos una propiedad sobre A en una sobre N. Veamos el siguiente ejemplo sobre árboles binarios.

---------
data Bin = Null | Leaf | Node Bin Bin

cantleaf :: Bin -> Int
cantleaf Null = 0
cantleaf Leaf = 1
cantleaf (Node t u) = cantleaf t + cantleaf u

cantnode :: Bin -> Int
cantnode (Node t u) = 1 + cantnode t + cantnode u
cantnode _ = 0

---------
Queremos probar que ∀t :: Bin: cantleaf t <= cantnode t + 1.

Definimos la siguiente propiedad:
Q(n) = height(t) = n => cantleaf t <= cantnode t + 1 (para todo t :: Bin).

height :: Bin -> Int
height (Node t u) = 1 + max (height t) (height u)
height _ = 0

---------
Usamos la 2da forma de inducción y suponemos (HI) que ∀i < n, si height(t) = i, entonces cantleaf t <= cantleaf t + 1.

Hacemos un análisis por casos de n:
- Si n = 0, entonces la HI no se aplica y debemos probar directamente: si height(t) = 0 entonces por <def height> t tiene una hoja o está vacío:
    - Tiene una hoja: cantleaf t = 1 <= cantnode t + 1 = 0 + 1 = 1
    - Es vacío: cantleaf t = 0 <= cantnode t + 1 = 0 + 1 = 1

- Si n > 0 y height(t) = n entonces podemos calcular
= cantleaf t                            <height(t) > 0>
= cantleaf (Node u v)                   <cantleaf 3era def>
= cantleaf u + cantleaf v               <HI (height u < n) y (height v < n)>
<= cantnode u + 1 + cantnode v + 1  
= (1 + cantnode u + cantnode v) + 1     <cantnode 1era def>
= cantnode (Node u v) + 1

Y luego, por inducción hemos probado que cantleaf t <= cantnode t + 1. Así, hemos podido probar una propiedad sobre árboles usando inducción sobre naturales.

Sin embargo, es más práctico hacer inducción directamente sobre la estructura del árbol, utilizando inducción estructural.

________________________________________________
INDUCCIÓN ESTRUCTURAL
Definición. Dada una propiedad P sobre un tipo de datos algebraico T, para probar que P(t) vale para todo t de T:
- probamos P(T) para todo t dado por un constructor no recursivo.
- para todo t dado por un constructor con instancias recursivas t1, ... tk, probamos que si vale P(ti) para i = 1, ... k, entonces vale P(t).

Podemos definir una forma adicional de inducción estructural en la que suponemos que P(t') vale para todo t' :: T que ocurre dentro de t.

Veamos el siguiente ejemplo de inducción estructural para Bin:
data Bin = Null | Leaf | Node Bin Bin

---------
Definición. Inducción estructural para Bin.
Dada un propiedad P sobre elementos de Bin, para probar que P(t) vale para ∀t :: Bin:
- probamos P(Null) y P(Leaf).
- probamos que si P(u) y P(v) valen, entonces P(Node u v) vale.

---------
Probaremos la misma propiedad que antes: P(t): cantleaf t <= cantnode t + 1 ∀t :: Bin.

- Caso Null: cantleaf Null = 0 <= 1 = 0 + 1 = cantnode Null + 1
- Caso Leaf: cantleaf Leaf = 1 <= 1 = 0 + 1 = cantnode leaf + 1
- Caso Node u v:

La hipótesis inductiva es:
1) cantleaf u <= cantnode u + 1
2) cantleaf v <= cantnode v + 1

= cantleaf (Node u v)                   <cantleaf 3era def>
= cantleaf u + cantleaf v               <HI>
<= cantnode u + 1 + cantnode v + 1
= (1 + cantnode u + cantnode v) + 1     <cantnode 1era def>
= cantnode (Node u v) + 1

Y tenemos entonces que cantleaf (Noude u v) <= cantnode (Node u v) + 1
Así, hemos probado que la propiedad P vale para todo árbol t :: Bin.

________________________________________________
INDUCCIÓN ESTRUCTURAL PARA LISTAS
Definición. Inducción estructural para listas.
Dada una propiedad P sobre listas, para probar que vale P(xs) ∀xs :: [a]:
- probamos P([])
- probamos que si vale P(xs) entonces vale P(x:xs).

---------
Ejercicio. Probar que reverse (xs ++ ys) = reverse ys ++ reverse xs.
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

Probaremos por inducción estructural para listas sobre xs.
- Caso xs = []: 
    = reverse ([] ++ ys)        <++ 1era def>
    = reverse ys                <++ 1era def>
    = reverse ys ++ []          <reverse 1era def>
    = reverse ys ++ reverse []

- Caso xs: suponemos que P(xs) es verdadero: reverse (xs ++ ys) = reverse ys ++ reverse xs (HI), y probaremos P(x:xs).
    = reverse (x:xs ++ ys)              <reverse 2da def>
    = reverse (xs ++ ys) ++ [x]         <HI>
    = reverse ys ++ reverse xs ++ [x]   <reverse 2da def (al otro lado)>
    = reverse ys ++ reverse (x:xs)

---------
Ejercicio. Expresar la inducción estructural para el tipo Nat.
data Nat = Zero | Succ Nat

Dada una propiedad P sobre Nat, para probar que vale P(n) ∀n :: Nat:
- probamos P(Zero)
- probamos que si vale P(n) entonces vale P(Succ n).

________________________________________________
EJEMPLO: COMPILADOR CORRECTO
Dado un lenguaje aritmético simple, cuyo AST es:
data Expr = Val Int | Add Expr Expr

Su semántica denotacional está dada por el siguiente evaluador:
eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

Queremos compilar el lenguaje a la siguiente máquina de stack:
type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n:c) s = exec c (n:s)
exec (ADD :c) (m:n:s) = exec c (n + m :s)

Definimos un compilador:
comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

---------
Ejemplo: 
> e = Add (Add (Val 2) (Val 3)) (Val 4)
> eval e = 2 + 3 + 4 = 9

> comp e    = comp (Add (Val 2) (Val 3)) ++ comp (Val 4) ++ [ADD]
            = (comp (Val 2) ++ comp (Val 3) ++ [ADD]) ++ [PUSH 4] ++ [ADD]
            = [PUSH 2] ++ [PUSH 3] ++ [ADD] ++ [PUSH 4] ++ [ADD]
            = [PUSH 2, PUSH 3, ADD, PUSH 4, ADD]

> exec (comp e) []  = exec [PUSH 2, PUSH 3, ADD, PUSH 4, ADD] []
                    = exec [PUSH 3, ADD, PUSH 4, ADD] [2]
                    = exec [ADD, PUSH 4, ADD] [3, 2]
                    = exec [PUSH 4, ADD] [5]
                    = exec [ADD] [4, 5]
                    = [9]

---------
El compilador es correcto si ∀e exec (comp e) [] = [eval e].
Debemos probar esto por inducción estructural de Expr.

Definición. Inducción estructural para Expr.
Dada una propiedad P sobre elementos de Expr, para probar que ∀e :: Expr vale P(e):
- probamos P(Val n) para todo n.
- probamos que si P(e) y P(e') valen, entonces P(Add e e') también vale.

---------
Queremos probar P(e): exec(comp e) [] = [eval e] (∀e :: Expr).
Lo hacemos por inducción estructural sobre e.

- Caso e = (Val n):
    = exec (comp (Val n)) []    <comp 1era def>
    = exec [PUSH n] []          <exec 2da def>
    = exec [] [n]               <exec 1era def>
    = [n]                       <eval 1era def (al otro lado)>
    = [eval (Val n)]

- Caso e = (Add e1 e2).
HI1: exec (comp e1) [] = [eval e1]
HI2: exec (comp e2) [] = [eval e2]

    = exec (comp (Add e1 e2)) []                        <comp 2da def>
    = exec (comp e1 ++ comp e2 ++ [ADD]) []             <Lema 1>
    = exec (comp e2 ++ [ADD]) (exec (comp e1) [])       <Lema 1>
    = exec [ADD] (exec (comp e2) (exec (comp e1) []))   <HI>
    = exec [ADD] (exec (comp e2) [eval e1])
    = ? No puedo seguir. Necesito generalizar la hipótesis inductiva.

---------
Probamos una propiedad más fuerte => La hipótesis inductiva será más fuerte.
Generalizamos: 
∀e exec (comp e) [] = [eval e] ==> ∀e,s exec (comp e) s = (eval e):s

La prueba original es un caso particular de la general (cuando s = []).

---------
Prueba: el compilador es correcto (propiedad general).
Queremos probar P(e): exec (comp e) s = (eval e):s (∀e,s).
Lo hacemos por inducción estructural sobre e.

Caso e = (Val n):
    = exec (comp (Val n)) s     <comp 1era def>
    = exec [PUSH n] s           <exec 2da def>
    = exec [] (n:s)             <exec 1era def>
    = (n:s)                     <eval 1era def (al otro lado)>
    = eval (Val n):s

Caso e = (Add e1 e2):
HI1: exec (comp e1) s = eval e1 : s ∀s
HI2: exec (comp e2) s = eval e2 : s ∀s

    = exec (comp (Add e1 e2)) s                     <comp 2da def>
    = exec (comp e1 ++ comp e2 ++ [ADD]) s          <Lema1>
    = exec (comp e2 ++ [ADD]) (exec (comp e1)) s    <Lema1>
    = exec [ADD] (exec (comp e2) (exec (comp e1) s))<HI1>
    = exec [ADD] (exec (comp e2) (eval e1 : s))     <HI2>
    = exec [ADD] (eval e2 : eval e1 : s)            <exec 3era def>
    = exec [] (eval e1 + eval e2 : s)               <exec 1era def>
    = (eval e1 + eval e2 : s)                       <eval 2da def>
    = eval (Add e1 e2) : s

---------
Nos queda probar el lema auxiliar.
Lema 1: exec (c ++ d) s = exec d (exec c s) ∀c,d,s

Donde para probar esto debemos asumir que el stack está bien formado, (para todo ADD hay al menos dos elementos en el stack).

-}