{-
4) El TAD priority queue es una cola en la cual cada elemento tiene asociado un valor que es su prioridad (a dos elementos distintos le corresponden prioridades distintas).
Los valores que definen la prioridad de los elementos pertenecen a un conjunto ordenado. Las siguientes son las operaciones soportadas por este TAD:

vacia: Construye una priority queue vacía.
poner: agrega un elemento a una priority queue con una prioridad dada.
primero: devuelve el elemento con mayor prioridad de una priority queue.
sacar: elimina de una priority queue el elemento con mayor prioridad.
esVacia: determina si una priority queue es vacía.
union: une dos priority queues.

---------------------------------------
-- OPERACIONES
tad PriorityQueue (Elem:Set, Priority:Ord) where
    vacia : PriorityQueue Elem Priority
    poner : Elem -> Priority -> PriorityQueue Elem Priority -> PriorityQueue Elem Priority
    primero : PriorityQueue Elem Priority -> Elem
    sacar: PriorityQueue Elem Priority -> PriorityQueue Elem Priority
    esVacia : PriorityQueue Elem Priority -> Bool
    union : PriorityQueue Elem Priority -> PriorityQueue Elem Priority -> PriorityQueue Elem Priority

---------------------------------------
-- ESPECIFACIONES ALGEBRAICAS / AXIOMAS

esVacia vacia = True

esVacia (poner e p pq) = False
primero (poner e p vacia) = e
primero (poner e1 p1 (poner e2 p2 pq)) = 
    if p1 >= p2
        then primero (poner e1 p1 pq)
        else primero (poner e2 p2 pq)

esVacia (sacar (poner e p pq)) = esVacia pq
sacar (poner e p vacia) = vacia
sacar (poner e1 p1 (poner e2 p2 pq)) = 
    if p1 >= p2
        then poner e2 p2 (sacar (poner e1 p1 pq))
        else poner e1 p1 (sacar (poner e2 p2 pq))
primero (sacar (poner e1 p1 (poner e2 p2 pq))) =
    if p1 >= p2
        then primero (poner e2 p2 (sacar (poner e1 p1 pq)))
        else primero (poner e1 p1 (sacar (poner e2 p2 pq)))

esVacia (union pq1 pq2) = esVacia pq1 && esVacia pq2
union vacia vacia = vacia
union pq vacia = pq
union vacia pq = pq
union pq vacia = union vacia pq
union (poner e1 p1 pq1) (poner e2 p2 pq2) = 
    if e1 == e2
        then union (poner e1 p1 pq1) pq2
        else poner e1 p1 (poner e2 p2 (union pq1 pq2))
primero (union pq1 pq2) =
    if esVacia pq1
        then primero pq2
        else if esVacia pq2
            then primero pq1
            else if priority (primero pq1) >= priority (primero pq2)
                then primero pq1
                else primero pq2
sacar (union pq1 pq2) =
    if esVacia pq1
        then sacar pq2
        else if esVacia pq2
            then sacar pq1
            else if priority (primero pq1) >= priority (primero pq2)
                then union (sacar pq1) pq2
                else union pq1 (sacar pq2)


---------------------------------------
ESPECIFICACIÓN TOMANDO COMO MODELO LOS CONJUNTOS (ejercicio 3)
PriorityQueue = Conjunto de duplas (elemento, prioridad).

vacia = vacio
esVacia vacio = esVacio vacio
esVacia (insertar (e, p) c) = esVacio (insertar (e, p) c)

poner e p vacio = insertar (e, p) vacio     -- vacio es un conjunto
poner e p c = insertar (e, p) c             -- c es un conjunto

primero (insertar (e, p) vacio) = insertar (e, p) vacio
primero (insertar (e1, p1) (insertar (e2, p2) c)) = 
    if p1 >= p2
        then primero (insertar (e1, p1) c)
        else primero (insertar (e2, p2) c)

sacar (insertar (e, p) vacio) = vacio
sacar (insertar (e1, p1) (insertar (e2, p2) c)) =
    if p1 >= p2
        then insertar (e2, p2) (sacar (insertar (e1, p1) c))
        else insertar (e1, p1) (sacar (insertar (e2, p2) c))
primero (sacar (insertar (e1, p1) (insertar (e2, p2) c))) =
    if p1 >= p2
        then primero (insertar (e2, p2) (sacar (insertar (e1, p1) c)))
        else primero (insertar (e1, p1) (sacar (insertar (e2, p2) c)))

union vacio = vacio
union c vacio = c
union vacio c = c
union (insertar (e1, p1) c1) (insertar (e2, p2) c2) =
    if e1 == e2
        then union (insertar (e1, p1) c1) c2
        else insertar (e1, p1) (insertar (e2, p2) (union c1 c2))

primero (union c1 c2) =
    if esVacio c1
        then primero c2
        else if esVacio c2
            then primero c1
            else if priority (primero c1) >= priority (primero c2)
                then primero c1
                else primero c2

sacar (union c1 c2) =
    if esVacio c1
        then sacar c2
        else if esVacio c2
            then sacar c1
            else if priority (primero c1) >= priority (primero c2)
                then union (sacar c1) c2
                else union c1 (sacar c2)
-}