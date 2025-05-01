{-
TIPOS ABSTRACTOS DE DATOS (TAD)
Los tipos abstractos de datos (TAD) son una forma de estructurar y organizar datos en programación. Definen un conjunto de valores y un conjunto de operaciones que pueden realizarse con esos valores, pero ocultan los detalles internos de cómo se implementan esos valores y operaciones.

Esto permite que los usuarios utilicen los tipos de datos sin necesidad de conocer los detalles de su implementación subyacente, lo que promueve la modularidad y la abstracción en el diseño de programas.

- La idea de un tipo asbtracto de datos es abstraer detalles de implementación.
- El usuario es alguien que simplemente usa la abstracción.
- El implementador provee una implementación que se ajusta al comportamiento esperado.
- El usuario solo puede suponer el comportamiento descripto.
- Podemos ser más precisos sobre el comportamiento mediante una especificacion.

_______________________
EJEMPLO: COLAS
Una cola es una estructura a la cual
- Podemos agregar elementos.
- Podemos obtener el primer elemento.
- Podemos quitar el primer elemento.
- Podemos preguntar si está vacía.
- Existe una relación entre el orden en que se agregan elementos y se sacan (FIFO).

Esta descripción es abstracta porque refleja el comportamiento y no la implementación.

_______________________
TADS
Un TAD consiste de:
1. Un nombre de tipo. Ej: Cola
2. Operaciones.
    tad Cola (A:Set) where
        import Bool
        vacia : Cola A
        poner : A -> Cola A -> Cola A
        primero : Cola A -> A
        sacar : Cola A -> Cola A
        esVacia : Cola A -> Bool
3. Especificación del comportamiento
    - Especificación algebraica: se describen operaciones y ecuaciones entre operaciones.
    - Modelos: se describen operaciones y cómo se interpretan en un modelo matemático.

_______________________
EJEMPLO: Especificación algebraica para colas.
- esVacia vacia = True
- esVacia (poner x q) = False
- primero (poner x vacia) = x
- primero (poner x (poner y q)) = primero (poner y q)
- sacar (poner x vacia) = vacia
- sacar (poner x (poner y q)) = poner x (sacar (poner y q))

Obs: no confundir especificación con implementación.

_______________________
ESPECIFICACIONES ALGEBRAICAS
Las especificaciones deben ser mediante ecuaciones.
Solo deben aparecer operaciones de TAD y variables libres (se suponen cuantificadas universalmente).

Incorrecto: size x =    0                   si esVacia x
                        1 + size (sacar x)  en otro caso

Correcto: size x = if (esVacia x) then 0 else 1 + size (sacar x)

- 0, 1 son operadores nularios (no toman argumentos)
- + es un operador binario de un TAD de naturales
- if then else es un operador ternario de un TAD de booleanos.

En algunos casos, puede quedar comportamiento sin definir. Por ejemplo, que dice la especificación sobre <primero vacia>?

_______________________
MODELOS
Como modelo de colas tomamos las secuencias <x1, x2, ... xn>
Para cada operación damos una función equivalente sobre modelos:
- vacia = <>
- poner x <x1, x2, ... xn> = <x, x1, x2, ... xn>
- sacar <x1, x2, ... xn> = <x1, x2, ... xn-1>
- primero <x1, x2, ... xn> = xn
- esVacia <x1, x2, ... xn> = True si n = 0
- esVacia <x1, x2, ... xn> = False en otro caso

TRABAJANDO CON MODELOS
A cada cola c, le corresponde un modelo [c].
A cada operación op, le corresponde una operación [op] que trabaja sobre el modelo.

Una implementación se ajusta al modelo si: [op c] = [op] [c]
Por ejemplo, esperamos que [sacar c] = [sacar] [c]

_______________________
IMPLEMENTACIONES
Implementación del TAD de Colas usando listas en Haskell.
Hay dos posibles implementaciones con listas.
    1. Se agregan elementos al final de la lista, se sacan de la cabeza.
    2. Se agregan elementos a la cabeza, se sacan del final.

Los dos métodos son lineales en alguna operación. Como implementamos una cola eficiente?
-}

-----------------------
-- 1era implementación: se agregan elementos al final de la lista, se sacan de la cabeza.
type Cola a = [a]

-- Wvacia = O(1)
vacia :: Cola a
vacia = []

-- Wponer = O(n)
poner :: a -> Cola a -> Cola a
poner v [] = [v]
poner v (x:xs) = x : (poner v xs)

-- Wprimero = O(1)
primero :: Cola a -> a
primero [] = error "Lista vacía"
primero (x:xs) = x

-- Wsacar = O(1)
sacar :: Cola a -> Cola a
sacar [] = error "Lista vacía"
sacar (x:xs) = xs

-- Wesvacia = O(1)
esVacia :: Cola a -> Bool
esVacia xs = null xs

-- poner 3 (poner 2 (poner 1 vacia)) = [1,2,3]
-- primero (poner 3 (poner 2 (poner 1 vacia))) = 1
-- sacar (poner 3 (poner 2 (poner 1 vacia))) = [2,3]

-----------------------
-- 2da implementación: se agregan elementos a la cabeza, se sacan del final.
type Cola a = [a]

-- Wvacia = O(1)
vacia :: Cola a
vacia = []

-- Wponer = O(1)
poner :: a -> Cola a -> Cola a
poner x xs = x:xs

-- Wprimero = O(1)
primero :: Cola a -> a
primero [] = error "Cola vacía"
primero (x:xs) = last (x:xs)

-- Wsacar = O(n)
sacar :: Cola a -> Cola a
sacar [] = error "Cola vacía"
sacar [x] = vacia
sacar (x:xs) = x : (sacar xs)

-- Wesvacia = O(1)
esVacia :: Cola a -> Bool
esVacia xs = null xs

-- poner 1 (poner 2 (poner 3 vacia)) = [1,2,3]
-- primero (poner 1 (poner 2 (poner 3 vacia))) = 3
-- sacar (poner 1 (poner 2 (poner 3 vacia))) = [1,2]

-----------------------
{- 3er variante de implementación
Implementemos colas usando un par de listas (xs, ys) tal que los elementos en orden sean xs ++ reverse ys.

Invariante de la implementación: Si xs es vacía, entonces ys también (las operaciones deben conservar este invariante)

Ejemplo: <1,2,3,4,5> podría ser ([1,2], [5,4,3]) o ([1], [5,4,3,2])
-}

type Cola a = ([a], [a])

-- Wvacia = O(1)
vacia :: Cola a
vacia = ([],[])

-- Wponer = O(1)
poner :: a -> Cola a -> Cola a
poner x (ys, zs) = validar (ys, x:zs)

-- Wprimero = O(1)
primero :: Cola a -> a
primero (x:xs, ys) = x

-- Wsacar = O(|ys|), O(1) (amortizado)
sacar :: Cola a -> Cola a
sacar (x:xs, ys) = validar(xs, ys)

-- Wesvacia = O(1)
esVacia :: Cola a -> Bool
esVacia (xs, ys) = null xs

validar :: Cola a -> Cola a
validar (xs, ys) = 
    if null xs
        then (reverse ys, [])
        else (xs, ys)

-- Obs: el costo amortizado de una operación es una medida promedio del costo de realizar una secuencia de operaciones en una estructura de datos durante un período de tiempo. 
-- El reverse es O(n) pero se hace cada tanto, solo en algunos pocos casos. Por lo tanto, en los otros casos las operaciones son O(1), y así nos estamos ahorrando costo para cuando tengamos que aplicar el reverse que es O(n).
-- Es decir, la operacion sacar termina siendo O(1), pues hemos acumulado tantos O(1) (como monedas) al momento de costear una operación O(n).


{-
_______________________
ESPECIFICACIÓN DE COSTO
Cada TAD admite diferentes implementaciones. En cada implementación, las operaciones pueden tener diferentes costos (trabajo, profundidad).

Dependiendo del uso de la estructura, puede convenir una implementación u otra. Por lo tanto, es importante tener una especificación de costo de cada implementación.

_______________________
TADS EN HASKELL
Una forma de implementar un TAD en Haskell es mediante una clase de tipos.
class Cola t where
    vacia :: t a
    poner :: a -> t a -> t a
    sacar :: t a -> t a
    primero :: t a -> a
    esVacia :: t a -> Bool

Una implementación es una instancia
instance Cola [] where
    vacia = []
    poner x xs = x:xs
    sacar xs = init xs
    primero = last
    esVacia xs = null xs

Usamos un TAD con una función polimórfica en el TAD
ciclar :: Cola t => Int -> t a -> t a
ciclar 0 cola = cola
ciclar n cola = ciclar (n-1) (poner (primero cola) (sacar cola))

Notar que la función ciclar funciona para cualquier instancia de Cola. O, lo que es lo mismo, funciona para cualquier implementación del TAD.
La función ciclar no puede suponer nada acerca de la implementación.

_______________________
CONCLUSION
ESPECIFICACIÓN: refiere a qué operaciones tiene el TAD y cómo se comportan. Es única.

IMPLEMENTACIÓN: refiere a cómo se realizan las operaciones y cuánto cuestan. Puede haber varias implementaciones (con diferentes costos). Todas deben garantizar el comportamiento dado por la especificación.

USO: solo puede suponer el comportamiento dado por la especificación. Se elige implementación de acuerdo al uso (menor costo para un determinado uso).

TIPOS ABSTRACTOS DE DATOS: ocultan detalles de implementación, el comportamiento se describre algebraicamente o proveyendo un modelo y cada implementación debe tener una especificación de costo.
-}

{-
TAD BOOLEANO
Operaciones:
tad Bool where
    true : Bool
    false : Bool
    not : Bool -> Bool
    and : Bool -> Bool -> Bool
    or : Bool -> Bool -> Bool
    => : Bool -> Bool -> Bool
    if _ then _ else _ : Bool -> E -> E -> E

Especificaciones algebraicas:
not true = false
not false = true
and true x = x
and false x = false
or true x = true
or false x = x
true => x = x
false => x = true
if true then x else y = x
if false then x else y = y
-}

{-
TAD NATURALES
Operaciones:
0 : Nat
succ : Nat -> Nat
(>) : Nat -> Nat -> Bool
(=) : Nat -> Nat -> Bool
(+) : Nat -> Nat -> Nat
(-) : Nat -> Nat -> Nat

Especificaciones algebraicas:
x + 0 = x
x + (succ y) = succ(x + y)
0 - x = 0
x - 0 = x
(succ x) - (succ y) = x - y
0 (=) 0 = true
(succ x) (=) 0 = false
0 (=) (succ y) = false
0 > x = false
(succ x) > 0 = true
(succ x) > (succ y) = x > y
-}