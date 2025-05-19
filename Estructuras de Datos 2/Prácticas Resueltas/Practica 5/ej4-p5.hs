{-
4) Operaciones soportadas por el TAD:
• vacia: Construye una priority queue vacia.
• poner: Agrega un elemento a una priority queue con una prioridad dada.
• primero: Devuelve el elemento con mayor prioridad de una priority queue.
• sacar: Elimina de una priority queue el elemento con mayor prioridad.
• esVacia: Determina si una priority queue es vacia.
• union: Une dos priority queues.

___________________________________________________
tad PriorityQueue (A:Type, P:OrderedSet) where
    vacia : PriorityQueue A P
    poner : A -> P -> PriorityQueue A P
    primero : PriorityQueue A P -> A
    sacar : PriorityQueue A P -> PriorityQueue A P
    esVacia : PriorityQueue A P -> Bool
    union : PriorityQueue A P -> PriorityQueue A P -> PriorityQueue A P

___________________________________________________
a) Especificaciones algebraicas.

primero (poner x p vacia) = x

primero (poner x p (poner y q qs)) =
    if (p >= q)
        then primero (poner x p qs)
        else primero (poner y q qs)

sacar (poner x p vacia) = vacia

sacar (poner x p (poner y q qs)) =
    if (p == maximo(qs))
        then qs
        else poner x p (sacar qs)

esVacia vacia = True
esVacia (poner x p pq) = False

union vacia pq = pq
union pq vacia = pq
union (poner x p pq1) pq2 = poner x p (union qs1 qs2)

___________________________________________________
b) Especificación tomando como modelo los conjuntos.
- ∅: conjunto vacío.
- {x1, ... xn}: conjunto con n elementos.
- ∪: unión de conjuntos.

vacia = ∅
poner x p vacia = {(x,p)}
poner x p pq = {(x,p)} ∪ pq
primero {(x,p)} = x
...
-}