{-
SECUENCIAS
Seq es un TAD para representar secuencias de elementos. A continuación veremos algunas de sus operaciones y las especificaremos en términos de la noción matemática de secuencias.

1) Denotaremos la longitud (cant. de elementos) de una secuencia s como |s|.

2) Denotamos las secuencias como <x0, x1, ... xn>.

3) Un índice i es válido en s si 0 <= i < |s|.

4) Para i válido y secuencia s, notamos con si a su i-ésima proyección.

5) s' es subsecuencia de s si existe secuencia I de índices válidos en s estrictamente crecientes tal que si' = sIi.


____________________________________
OPERACIONES
Sea s una secuencia, y x un elemento.

- empty : Seq a
Representa la secuencia <>

- singleton : a -> Seq a
singleton x devuelve la secuencia <x>

- length : Seq a -> Nat
length s devuelve |s| (longitud de la secuencia).

- nth : Seq a -> Nat -> a
Si i es un índice válido, entonces nth s i devuelve la i-ésima posición de s.

- toSeq : [a] -> Seq a
Dada una lista xs, toSeq xs nos devuelve la representación de xs como una secuencia (respetando índices).


____________________________________
OPERACIONES DE ALTO ORDEN

- tabulate : (Nat -> a) -> Nat -> Seq a
tabulate f n devuelve la secuencia <f 0, f 1, f 2, ... f (n-1)>

- map : (a -> b) -> Seq a -> Seq b
map f s devuelve la secuencia <f s0, f s1, f s2, ...>

- filter : (a -> Bool) -> Seq a -> Seq a
filter p s devuelve la subsecuencia más larga en la que p vale para todos sus elementos.

- append : Seq a -> Seq a -> Seq a
append s t es la secuencia que une las secuencias s y t: <s0, s1, ... sn, t0, t1, ... tn>

- take : Seq a -> Nat -> Seq a
take s n devuelve la secuencia hasta el n índice si n < |s|, o devuelve la misma secuencia si el n argumento es mayor que la longitud de la secuencia.

- drop : Seq a -> Nat -> Seq a
drop s n devuelve la secuencia que es igual a s pero sin los primeros sn-1 índices: <sn, ... s|s|-1>


____________________________________
VISTA DE UNA SECUENCIA COMO UN ÁRBOL
La siguiente operación nos permite ver a las secuencias como si fueran un arbol. La idea de descomponer la secuencia en una estructura de árbol es para facilitar ciertas operaciones.

showt : Seq a -> TreeView a

data TreeView a = EMPTY | ELT a | NODE (Seq a) (Seq a)

- Si |s| = 0, showt s evalúa a EMPTY.
- Si |s| = 1, showt s evalúa a ELT s0.
- Si |s| > 1, showt s evalúa a NODE (take s |s|/2) (drop s |s|/2)
- Notar que TreeView no es un tipo recursivo.

Ejemplo: supongamos que tenemos la secuencia s = <1, 2, 3, 4, 5, 6>
s1 = showt s -> NODE <1, 2, 3> <4, 5, 6>


____________________________________
OPERACIÓN FOLDR

foldr : (a -> b -> b) -> b -> Seq a -> b
- (a -> b -> b): es una función de 2 argumentos que toma un elemento de la secuencia (a) y un acumulador (b), y devuelve un nuevo acumulador (b).
- b: es el valor inicial del acumulador.
- Seq a: es la secuencia de elementos que se va a procesar.
- b: es el valor resultante después de aplicar la función a todos los elementos de la secuencia.

Ejemplo: foldr (+) e s = s0 + (s1 + (... (sn + e)))
- e es el valor inicial del acumulador, en este caso la secuencia vacía.
- foldr va aplicando la función + a todos los elementos de la secuencia, en este caso de derecha a izquierda (por la r de foldR).

FOLDL: evalua de izquierda a derecha.
foldl : (b -> a -> b) -> b -> Seq a -> b


____________________________________
OPERACIÓN REDUCE

El reduce permite realizar operaciones que son más paralelizables que foldr y foldl.

reduce : (a -> a -> a) -> a -> Seq a -> a
- (a -> a -> a): toma dos elementos del mismo tipo y devuelve un solo elemento del mismo tipo.
- elemento neutro a: este es el valor que se utiliza cuando se reduce una secuencia vacía. Cuando la secuencia es vacía, reduce devuelve el valor neutro e.
- Seq a: es la secuencia de elementos que se van a reducir.

La función reduce se utiliza para reducir una secuencia de valores a un único valor. Recibe como argumento una función que define como se va a combinar cada elemento de la secuencia para obtener un resultado final.

Por ejemplo, podemos realizar operaciones como la suma de los elementos, encontrar el máximo o mínimo, etc.

-------------
Ejemplo
Consideremos la siguiente secuencia s = <1, 2, 3, 4>, la función binaria + y el elemento neutro 0.

reduce (+) 0 s
Si la secuencia s es no vacía, el reduce combina los elementos de la siguiente manera:

Para foldr, el orden de combinación es 1 + (2 + (3 + 4))
Para foldl, el orden de combinación es (((1 + 2) + 3) + 4)

-------------
Comportamiento para Funciones Asociativas y No Asociativas
- Funciones Asociativas: Si la función es asociativa, como la suma de enteros, el resultado de reduce será el mismo independientemente del orden en que se combinen los elementos.
- Funciones No Asociativas: Si la función no es asociativa, como la suma de números en punto flotante, el resultado puede variar dependiendo del orden de reducción.

Limitarse a funciones asociativas no es una buena idea. La suma y multiplicación de punto flotante no es asociativa, por ejemplo. 

-------------
Especificación del Orden de Reducción
Para definir reduce adecuadamente, debemos especificar cómo se combinan los elementos, ya que el orden de combinación puede afectar el resultado para funciones no asociativas. El orden de reducción es parte del TAD (Tipo Abstracto de Datos) y debe ser definido claramente.


____________________________________
ORDEN DE REDUCCIÓN DE REDUCE
Definiremos la operación reduce usando un árbol de combinación para poder especificar el orden de reducción de una secuencia de manera clara y eficiente.

Un árbol de combinación es una estructura de datos utilizada en algoritmos de división y conquista para procesar eficientemente grandes conjuntos de datos. En el contexto del cálculo paralelo y distribuido, los árboles de combinación son especialmente útiles para paralelizar y optimizar operaciones como la reducción, la búsqueda y la clasificación.

En esencia, un árbol de combinación es un árbol binario que se utiliza para dividir una tarea en partes más pequeñas y luego combinar los resultados de manera eficiente.

--------------
data Tree a = Leaf a | Node (Tree a) (Tree a)

toTree : Seq a -> Tree a
toTree s = case |s| of
    1 -> (Leaf s0)
    n -> Node (toTree (take s pp)) (toTree (drop s pp))
        where pp = 2^(ilg(n-1))

Donde ilg es el logaritmo entero. El propósito de pp es encontrar el tamaño más grande de una subsecuencia que equilibra el árbol de combinación. El primer subárbol buscará ser un árbol completo -> esto sirve para maximizar la cantidad de operaciones que podemos paralelizar.

Si |s| = 2^k, el resultado es un árbol binario perfecto.

--------------
Ahora podemos definir reduce sobre árboles:
reduceT (+) (Leaf x) = x
reduceT (+) (Node l r) = (reduceT (+) l) + (reduceT (+) r)

Y podemos especificar reduce para secuencias:
- Si |s| = 0, <reduce (+) b s> devuelve b.
- Si |s| > 0, y <reduceT (+) (toTree s)> devuelve v, entonces <reduce (+) b s> devuelve (b + v).

Con esto no estamos dando una implementación de reduce, solo especificando el orden de reducción. Ejemplo: s = <s0, s1, s2, s3, s4, s5, s6>
reduce (+) b s = b + (((so + s1) + (s2 + s3)) + ((s4 + s5) + s6))


____________________________________
DIVIDE & CONQUER CON REDUCE
Podemos definir un algoritmo divide and conquer genérico.
dyc s = case showt s of
    EMPTY -> val
    ELT v -> base v
    NODE l r -> let (l', r') = dyc l || dyc r
                in combine l' r'

En una línea usando reduce:
reduce combine val (map base s)


____________________________________
MAXIMA SUBSECUENCIA CONTIGUA CON REDUCE
La máxima subsecuencia contigua, también conocida como "máxima subsecuencia sumada", es una subsecuencia de números en una lista tal que la suma de sus elementos es máxima entre todas las subsecuencias posibles.

Por ejemplo, considera la lista de números:
<-2, 1, -3, 4, -1, 2, 1, -5, 4>
La máxima subsecuencia contigua en esta lista sería [4, -1, 2, 1], ya que la suma de estos elementos es 6, que es la mayor suma posible dentro de todas las subsecuencias contiguas en la lista.

--------------
El problema de encontrar la máxima subsecuencia contigua se puede resolver con el patrón dyc.

La solución devuelve una tupla con:
1. El resultado deseado: representa la máxima subsecuencia contigua.
2. El máximo prefijo: la máxima subsecuencia contigua que termina en la posición actual.
3. El máximo sufijo: la máxima subsecuencia contigua que comienza en la posición actual.
4. La suma total: la suma total de la subsecuencia actual.

val = (0, 0, 0, 0)
base v =
    let v' = max v 0
    in (v', v', v', v)
combine (m, p, s, t) (m', p', s', t') =
    (max (s + p') m m', max p (t + p'), max s' (s + t'), t + t')

Luego, la solución es:
reduce combine val (map base s)


____________________________________
OPERACIÓN SCAN

scan : (a -> a -> a) -> a -> Seq a -> (Seq a, a)
La operación scan es una variante de reduce que en lugar de devolver un solo valor, devuelve una secuencia de valores intermedios obtenidos durante el proceso de reducción. Esto permite ver cómo se acumulan los resultados parciales.

Ejemplo: scan (+) 0 <1, 2, 3, 4> -> (<0, 1, 3, 6, 10> 10)
- 0 | 0 + 1 | 0 + 1 + 2 | 0 + 1 + 2 + 3 | 0 + 1 + 2 + 3 + 4

Cuando (+) es asociativa, <scan (+) b s> es equivalente a:
scan (+) b s = (tabulate (\i -> reduce (+) b (take s i)) |s|, reduce (+) b s)


____________________________________
ARREGLOS PERSISTENTES
Los arreglos persistentes son estructuras de datos que no destruyen el arreglo original al realizar un cambio, sino que lo copian y le agregan la modificación a la copia. 

Al igual que los arrays normales, se puede acceder en tiempo constante a cualquier índice. Sin embargo, no se puede hacer updates destructivos.

Tienen operaciones para crear arreglos a partir de una lista, a partir de funciones y a partir de otros arreglos.

- length :: Arr a -> Int
length p devuelve el tamaño del arreglo p.
W(n) = O(1) y S(n) = O(1).

- nth :: Arr a -> Int -> a
nth p i devuelve el i-ésimo elemento del arreglo p.
W(n) = O(1) y S(n) = O(1).

- fromList :: [a] -> Arr a
construye un arreglo a partir de una lista.
W(n) = O(n) y S(n) = O(n).

- tabulate :: (Int -> a) -> Int -> Arr a
tabulate f n construye un arreglo p de tamaño n tal que
nth p i == f i.
W(n) = O(W(f 0) + W(f 1) + ...)
S(n) = O(max (f 0) (f 1) ...)

- subarray :: Arr a -> Int -> Int -> Arr a
subarray a i l construye el subarreglo de a que comienza en el índice i y es de longitud l.
W(n) = O(1) y S(n) = O(1).
La complejidad es constante porque en la estructura del arreglo tenemos un puntero al inicio y la longitud del arreglo. Por lo tanto, para obtener un subarreglo, simplemente se cambia el valor de la longitud del arreglo que pasamos como argumento.


____________________________________
ESPECIFICACIÓN DE COSTOS DE SECUENCIAS CON ARREGLOS
Para muchos arreglos persistentes, la longitud del arreglo se almacena explícitamente junto con el arreglo en la estructura de datos. Esto permite que la operación length simplemente acceda a este valor almacenado, en lugar de tener que recalcularlo. Debido a esto, la operación length puede ser realizada en tiempo constante O(1).

Podemos implementar secuencias usando arreglos con los siguientes costos:
- empty: 
    1. W(n) = O(1)
    2. S(n) = O(1)

- singleton: 
    1. W(n) = O(1)
    2. S(n) = O(1)

- length: 
    1. W(n) = O(1)
    2. S(n) = O(1)

- nth: 
    1. W(n) = O(1)
    2.S(n) = O(1)

- take s n:
    1. W(n) = O(1)
    2. S(n) = O(1)

- drop s n: 
    1. W(n) = O(1)
    2. S(n) = O(1)

- showt s: 
    1. W(n) = O(1)
    2. S(n) = O(1)

- append s t: 
    1. W(n) = O(|s| + |t|)
    2. S(n) = O(1)

- tabulate f n: 
    1. W(n) = O(W(f 0) + W(f 1) + ... W(f n-1))
    2. S(n) = O(max S(f x))
Esto se debe a que tabulate debe calcular f i para cada i y almacenar el resultado en el nuevo arreglo. Cada llamada a f se ejecuta una vez por cada índice de 0 a n-1.

- map f s:
    1. W(n) = O(W(f s0) + W(f s1) + ... W(f sn))
    2. S(n) = O(max S(f si))

- filter f s:
    1. W(n) = O(W(f s0) + ... W(f sn))
    2. S(n) = O( lg |s| + max S(f si) )

Suponemos que Wf = O(1).
- reduce (+) b s:
    1. W(n) = O(|s|)
    2. S(n) = O(lg |s|)

Suponemos que Wf = O(1).
- scan (+) b s:
    1. W(n) = O(|s|)
    2. S(n) = O(lg |s|)


____________________________________
IMPLEMENTACIÓN DE SCAN
Hemos visto que la implementacion de scan de la siguiente forma:

scan : (a -> a -> a) -> a -> Seq a -> (Seq a, a)
Cuando (+) es asociativa, <scan (+) b s> es equivalente a:
scan (+) b s = (tabulate (\i -> reduce (+) b (take s i)) |s|, reduce (+) b s)

Pero esta no es una implementación eficiente, suponiendo (+) = O(1). Es decir, scan parece ser inherentemente secuencial. ¿Cómo la implementamos en paralelo?

-----------
DIVIDE AND CONQUER vs CONTRACCIÓN
Pensemos como resolver el problema con Divide & Conquer:
- partimos la secuencia en dos y llamamos recursivamente.
- pero la mitad derecha depende de la izquierda!

CONTRACCIÓN DE ENTRADA
- Para resolver un problema, resolvemos una instancia más chica del mismo problema.
- A diferencia de D&C, hacemos solo una llamada recursiva.
- Tenemos los siguientes pasos:
    1. Contraemos la instancia del problema a una instancia (mucho) más chica (contracción).
    2. Resolvemos recursivamente la instancia chica.
    3. Usamos la solución de la instancia chica para resolver la grande (expansión).
- Útil en algoritmos paralelos: contracción y expansión son usualmente paralelizables y si contraemos a una fracción, la recursión tiene profundidad logarítmica.

-----------
EJEMPLO DE CONTRACCIÓN: reduce
1. Consideremos reduce + 0 <2, 3, 6, 2, 1, 4>
2. Contraemos la secuencia de la siguiente manera:
    <2+3, 6+2, 1+4> = <5, 8, 5>
3. Calculamos recursivamente reduce + 0 <5, 8, 5> = 18
4. En este caso la expansión es la función identidad. El resultado es 18.


____________________________________
COLLECT
Supongamos que trabajamos con secuencias Seq a, donde existe un orden total para los elementos en a. Esto induce un orden lexicográfico sobre secuencias.

Sea a un tipo con un orden total (el tipo de las claves), y sea b un tipo cualquiera (el tipo de los datos), entonces:

La función collect :: Seq (a, b) -> Seq (a, Seq b), recolecta todos los datos asociados a cada clave. La secuencia resultado estará ordenada según el orden de a.

Ejemplo: collect <(2, "A"), (1, "B"), (1, "C"), (2, "D")> = <(1, <"B", "C">, (2, <"A", "D">))

------
IMPLEMENTACIÓN Y COSTOS.

collect se puede implementar en dos pasos:
    1. Ordenar la entrada según las claves. Esto tiene como efecto juntar claves iguales.
    2. Juntar todos los valores de claves iguales.

Ordenar tiene W(n) = O(Wc n lg n) y S(n) = O(Sc lg^2 n), donde Wc y Sc es el trabajo y profunidad de la comparación de claves.

Juntar todas las claves es W(n) = O(n) y S(n) = O(lg n).

Por lo tanto, el costo está dominado por el costo de la ordenación.


____________________________________
MAP/COLLECT/REDUCE - Map/reduce de Google
Implica hacer un map, seguido de un collect, seguido de varios reduce.

Ejemplo: asociar a cada palabra clave, una secuencia de documentos donde ésta ocurre.
    - <map apv> se aplica a una secuencia de documentos, donde apv transforma cada documente en secuencias de pares claves/valor.
    - esto da una secuencia de pares clave/valor que se aplana con <join>
    - luego, se aplica collect
    - luego, cada par de clave y secuencia de valores es "reducido" a un resultado por <red>

Generalmente, apv y red son funciones secuenciales. El paralelismo viene de aplicarlas en un map:

mapCollectReduce apv red s = 
    let pairs = join (map apv s)
        groups = collect pairs
    in map red groups

¿Cuántas veces aparece una palabra en una colección de documentos?
apv d = map (\w -> (w, 1)) (words d)
red (w, s) = (w, reduce (+) 0 s)
countWords s = mapCollectReduce apv red s

-}