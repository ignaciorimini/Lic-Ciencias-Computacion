# 4. En el juego de los 8 números (8-puzzle) con la siguiente 
# configuración inicial:

# 2 8 3
# 1 6 4
# 7 - 5

# ---------------------------------------
# a. Resolverlo mediante la estrategia de búsqueda en profundidad 
# estableciendo algún límite apropiado de profundidad y controlando
# los nodos de estados repetidos.

# Para este ejercicio definiremos el Estado Meta como el modelo en espiral.
# Para resolverlo por DFS, estableceremos un límite de profundidad de 5.
# Suponemos un orden de movimientos del blanco (Arriba, Abajo, Izq, Der).

# 2 8 3 || 2 8 3 | 2 8 3 | 2 8 3
# 1 6 4 || 1 - 4 | 1 6 4 | 1 6 4
# 7 - 5 || 7 6 5 | - 7 5 | 7 5 -

# 2 8 3 || 2 - 3 | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 - 4 || 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 4
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

# 2 - 3 || - 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 8 4 || 1 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 4
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

# - 2 3 || 1 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 8 4 || - 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 4
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

# 1 2 3 || 1 2 3 | 1 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# - 8 4 || 8 - 4 | 7 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 4
# 7 6 5 || 7 6 5 | - 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

# 1 2 3 || Meta alcanzada
# 8 - 4 ||
# 7 6 5 ||

# El camino a la meta: (Arriba, Arriba, Izq, Abajo, Der)
# Se alcanzo con un camino de 5 aristas (profundidad 5).


# ---------------------------------------
# b. Desarrollar las etapas de búsqueda con el método A* considerando:
# h1 = cantidad de números fuera de lugar.
# h2 = distancia de Manhattan

# Comparar los resultados obtenidos con ambas heurísticas 
# (solución óptima, cantidad de nodos expandidos) eb este caso de resolución
# y analizar si este comportamiento es un caso particular o si se puede
# generalizar a otros casos del juego.

# ----------------
# A* con f = g + h
# - g: costo acumulado
# - h = h1 (números fuera de lugar)
# - Criterio de desempate: quien tiene mayor g.

# f=0+4=4 f=1+3=4 f=1+5=6 f=1+5=6 
# 2 8 3 || 2 8 3 | 2 8 3 | 2 8 3
# 1 6 4 || 1 - 4 | 1 6 4 | 1 6 4
# 7 - 5 || 7 6 5 | - 7 5 | 7 5 -

#         f=2+3=5 f=2+3=5 f=2+4=6 f=2+5=7 f=2+5=7
# 2 8 3 || 2 - 3 | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 - 4 || 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 4
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

#         f=3+2=5 f=3+4=7 f=2+3=5 f=2+4=6 f=2+5=7 f=2+5=7
# 2 - 3 || - 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 8 4 || 1 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 5
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

#         f=4+1=5 f=3+4=7 f=2+3=5 f=2+4=6 f=2+5=7 f=2+5=7
# - 2 3 || 1 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 8 4 || - 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 5
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

#         f=5+0=5 f=5+2=7 f=3+4=7 f=2+3=5 f=2+4=6 f=2+5=7 f=2+5=7
# 1 2 3 || 1 2 3 | 1 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# - 8 4 || 8 - 4 | 7 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 5
# 7 6 5 || 7 6 5 | - 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

# 1 2 3 || Meta alcanzada
# 8 - 4 
# 7 6 5

# El camino a la meta: (Arriba, Arriba, Izq, Abajo, Der)
# Se alcanzo con un camino de 5 aristas.

# ----------------
# A* con f = g + h
# - g: costo acumulado
# - h = h2 (distancia Manhattan, suma la distancia vertical y horizontal 
# de cada ficha a su posición final).
# - Criterio de desempate: quien tiene mayor g.

# Inicial: 1(dist 1), 2(dist 1), 8(dist 2), 6(dist 1) -> f= 0+5=5

# f=0+5=5 f=1+4=5 f=1+6=7 f=1+6=7
# 2 8 3 || 2 8 3 | 2 8 3 | 2 8 3
# 1 6 4 || 1 - 4 | 1 6 4 | 1 6 4
# 7 - 5 || 7 6 5 | - 7 5 | 7 5 -

#         f=2+3=5 f=2+5=7 f=2+5=7 f=1+6=7 f=1+6=7
# 2 8 3 || 2 - 3 | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 - 4 || 1 8 4 | - 1 4 | 1 4 - | 1 6 4 | 1 6 4
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

#         f=3+2=5 f=3+4=7 f=2+5=7 f=2+5=7 f=1+6=7 f=1+6=7
# 2 - 3 || - 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 8 4 || 1 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 5 | 1 6 4
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

#         f=4+1=5 f=3+4=7 f=2+5=7 f=2+5=7 f=1+6=7 f=1+6=7
# - 2 3 || 1 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# 1 8 4 || - 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 5 | 1 6 4
# 7 6 5 || 7 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

#         f=5+0=5 f=5+2=7 f=3+4=7 f=2+5=7 f=2+5=7 f=1+6=7 f=1+6=7
# 1 2 3 || 1 2 3 | 1 2 3 | 2 3 - | 2 8 3 | 2 8 3 | 2 8 3 | 2 8 3
# - 8 4 || 8 - 4 | 7 8 4 | 1 8 4 | - 1 4 | 1 4 - | 1 6 5 | 1 6 5
# 7 6 5 || 7 6 5 | - 6 5 | 7 6 5 | 7 6 5 | 7 6 5 | - 7 5 | 7 5 -

# 1 2 3 || Meta alcanzada
# 8 - 4
# 7 6 5

# El camino a la meta: (Arriba, Arriba, Izq, Abajo, Der)
# Se alcanzo con un camino de 5 aristas.

# ----------------
# Según las trazas, ambas heurísticas mantuvieron el valor de f constante
# lo que resultó en la expansión de los mismos estados principales
# y tuvieron el mismo costo.

# Este comportamiento es un caso particular y no se puede generalizar:
# - La distancia de Manhattan (h2) siempre domina a la cantidad de piezas
# fuera de lugar (h1), entonces h2(n) >= h1(n).

# - Generalmente, una heurística que domina a otra expandirá menos nodos.
# Al dar valores más altos y cercanos al costo real, h2 lorga
# podar o ignorar caminos mediocres que h1 si exploraría en problemas complejos.

# - Conclusión: si el estado inicial estuviera más desordenado (por ej, a 20
# pasos de la meta), notaríamos que h1 expande cientos de nodos más que h2
# antes de encontrar la solución.