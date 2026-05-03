# 2. Escribir en pseudocódigo el algoritmo para la búsqueda a lo ancho
# y en profundidad, a partir del algoritmo de búsqueda general y realizando
# las especificaciones necesarias para cada caso.

# Algoritmo BusquedaGeneral(problema, estrategia)
#   frontera = {estado_inicial}
#
#   mientras frontera no este vacía hacer:
#       nodo = extraer_según_estrategia(frontera)
#
#       si es_meta(nodo) entonces:
#           retornar nodo (éxito) 
#
#       hijos = expandir(nodo)
#       frontera = insertar_segun_estrategia(hijos, frontera)
#   fin mientras
#   retornar fallo
# Fin Algoritmo

# Búsqueda a lo Ancho (BFS): utiliza una estructura de cola FIFO
# (First-In, First-Out). Los nuevos nodos se insertan al final de la lista.

# Búsqueda en Profundidad (DFS): utiliza una estructura de pila LIFO
# (Last-In, First-Out). Los nuevos nodos se insertan al inicio de la lista.


# ---------------------------------------
# Dar la evolución de la Lista de Espera de Nodos, al aplicar las estrategias
# de búsqueda a lo ancho y en profundidad, al problema representado en el 
# siguiente árbol, donde I es el estado inicial y M es el estado meta.

# Búsqueda a lo Ancho (BFS) - Estrategia Cola FIFO.
# Paso | Nodo Expandido | Frontera
# 0    | -              | [I]
# 1    | I              | [A,B]
# 2    | A              | [B,C,D]
# 3    | B              | [C,D,E,F]
# 4    | C              | [D,E,F,G,H]
# 5    | D              | [E,F,G,H]
# 6    | E              | [F,G,H,M,JJ]
# 7    | F              | [G,H,M,JJ]
# 8    | G              | [H,M,JJ]
# 9    | H              | [M,JJ]
# 10   | M              | [JJ]

# Búsqueda en Profundidad (DFS) - Estrategia Pila LIFO
# Paso | Nodo Expandido | Frontera
# 0    | -              | [I]
# 1    | I              | [A,B]
# 2    | A              | [C,D,B]
# 3    | C              | [G,H,D,B]
# 4    | G              | [H,D,B]
# 5    | H              | [D,B]
# 6    | D              | [B]
# 7    | B              | [E,F]
# 8    | E              | [M,JJ,F]
# 9    | M              | [JJ,F]