# 6. Considérese el grafo dirigido de la figura, que representa un espacio
# de estados, siendo I el estado inicial y G1 y G2 los dos estados objetivo.
# El número que figura en cada estado corresponde al valor de una función
# heurística h' que estima el coste mínimo necesario para pasar de ese
# estado al objetivo más cercano. Cada arista está etiquetada con un
# número que representa el coste real de atravesar dicha arista.

# ---------------------------------------
# a. ¿Es h' una heurística admisible? Justifique.

# Definición: una heurística es admisible si nunca sobreestima el coste real
# para alcanzar el objetivo más cercano. Es decir, h(n) <= h*(n) para todo n,
# donde h*(n) es el coste mínimo real desde n hasta un estado objetivo.

# Análisis del nodo E.
# - Coste real desde E hasta G1: no existe camino directo.
# - Coste real desde E hasta G2:
#  1. E -> D -> G2 = 3 + 11 = 14
#  2. E -> D -> F -> G2 = 3 + 6 + 3 = 12
#  3. E -> F -> G2 = 10 + 3 = 13

# Vemos entonces que el coste mínimo real es h*(E) = 12, pero el valor
# de la heurísticaa es h(E) = 14. Por lo tanto, dado que h(E) > h*(E),
# la heurística sobreestima el coste real en el nodo E. Luego, h' no es una
# heurística admisible.

# Si el valor de h(E) hubiese sido menor o igual a 12 (por ejemplo, 10 u
# 11), ese nodo en particular habría cumplido con la condición de
# admisibilidad. Sin embargo, para que toda la función heurística h' sea
# considerada admisible, la regla debe cumplirse en todos y cada uno de los
# estados del grafo. No basta con que se cumpla en uno solo.


# ---------------------------------------
# b. Indicar qué estado objetivo se alcanzará (si es que se alcanza alguno),
# qué estados se expandirán y en qué orden para cada uno de los siguientes
# algoritmos de búsqueda:
# - Primero en profundidad (DFS)
# - A*
# - Escalada simple (Hill Climbing)

# Cuando dos nodos tengan las mismas características por el criterio de
# selección que se esté usando, se seleccionará el primero por orden
# alfabético. Además, se evitarán las repeticiones de estados.

# -------
# Búsqueda en Profundidad (DFS) - Estrategia Pila LIFO

# Paso | Nodo Expandido | Frontera
# 0    | -              | [I]
# 1    | I              | [B,C]
# 2    | B              | [D,G1,C]
# 3    | D              | [F,G2,G1,C]
# 4    | F              | [G2,G1,C]
# 5    | G2             | Meta alcanzada

# Se llega al nodo G2 con camino I -> B -> D -> F -> G2 de costo 4+5+6+3=18.
# Nodos expandidos: I,B,D,F,G2

# -------
# A* con f(n) = g(n) + h(n)
# En la frontera usamos tuplas (Estado, g(n), h(n), f(n))

# Paso | Nodo Expandido | Frontera
# 0    | -              | [(I,0,15,15)]
# 1    | I              | [(B,4,11,15),(C,1,14,15)]
# 2    | B              | [(C,1,14,15),(D,9,4,13),(G1,25,0,25)]
# 3    | D              | [(C,1,14,15),(G1,25,0,25),(F,15,3,18),(G2,20,0,20)]
# 4    | C              | [(G1,25,0,25),(F,15,3,18),(G2,20,0,20),(A,2,13,15),(E,8,14,22)]
# 5    | A              | [(G1,25,0,25),(F,15,3,18),(G2,20,0,20),(E,8,14,22)] (B ya cerrado)
# 6    | F              | [(G1,25,0,25),(E,8,14,22),(G2,18,0,18)]
# 7    | G2             | Meta alcanzada

# Se llega al nodo G2 con camino I -> B -> D -> F -> G2 de costo 18.
# Nodos expandidos: I,B,D,C,A,F,G2

# -------
# Hill Climbing
# Se mueve al primer sucesor alfabético que sea mejor (menor h) que el estado actual.
# En la frontera usamos tuplas (Estado, h(n)).

# Paso | Nodo Expandido | Frontera
# 0    | -              | [(I,15)]
# 1    | (I,15)         | [(B,11),(C,14)]
# 2    | (B,11)         | [(D,4),(G1,0)]
# 3    | (D,4)          | [(F,3),(G2,0)]
# 4    | (F,3)          | [(G2,0)]
# 5    | (G2,0)         | Meta alcanzada

# Se llega al nodo G2 con camino I -> B -> D -> F -> G2 de costo 18.
# Nodos expandidos: I,B,D,F,G2.

# -------
# Comparación de Expansión
# A* expandió más nodos que DFS y Hill Climbing para llegar al mismo estado # - DFS: Expandió 5 nodos (I, B, D, F, G2).
# - Hill Climbing: Expandió 5 nodos (I, B, D, F, G2).
# - A*: Expandió 7 nodos (I, B, D, C, A, F, G2).

# Aunque A* está diseñado para ser eficiente, su naturaleza le obliga a
# explorar todos los nodos que resulten "promisorios" (con valores de f
# bajos) en cualquier rama del grafo. En este caso, las ramas de C y A 
# tenían valores de f competitivos (f=15), lo que obligó a A* a explorarlas 
# antes de decidirse por el camino final.

# Por el contrario, DFS y Hill Climbing son algoritmos que se "comprometen"
# con una sola ruta basándose en reglas locales o estructurales, lo que en
# este grafo específico les permitió "tener suerte" y encontrar la meta de
# forma más directa, aunque no siempre garantizan encontrar el camino más
# corto en problemas más complejos.
