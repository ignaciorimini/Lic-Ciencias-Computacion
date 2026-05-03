# 5. Considerando el siguiente mapa de Rumania con las rutas existentes
# y las distancias en línea recta entre las distintas ciudades y Bucarest.

# ---------------------------------------
# a. Aplique el algoritmo de búsqueda de costo uniforme para encontrar
# una solución al problema de ir desde Arad a Bucarest.

# UCS se implementa como Best-First Search con f(n) = g(n).
# Expande los nodos basándose exclusivamente en el costo acumulado g(n).
# Siempre elige el nodo con el menor g(n) en la frontera.

# Paso | Nodo Expandido | Frontera
# 0    | -              | [(Arad,0)]
# 1    | Arad           | [(Zerind,75),(Timisoara,118),(Sibiu,140)]
# 2    | Zerind         | [(Timisoara,118),(Sibiu,140),(Oradea,75+71=146)]
# 3    | Timisoara      | [(Sibiu,140),(Oradea,146),(Lugoj,229)]
# 4    | Sibiu          | [(Oradea,146),(Lugoj,229),(Fagaras,239),(Rimnicu Vilcea,220)]
# 5    | Oradea         | [(Lugoj,229),(Fagaras,239),(Rimnicu Vilcea,220)]
# 6    | Rimnicu Vilcea | [(Lugoj,229),(Fagaras,239),(Pitesti,317),(Craiova,366)]
# 7    | Lugoj          | [(Fagaras,239),(Pitesti,317),(Craiova,366),(Mehadia,304)]
# 8    | Fagaras        | [(Pitesti,317),(Craiova,366),(Mehadia,304),(Bucarest,450)]
# 9    | Mehadia        | [(Pitesti,317),(Craiova,366),(Bucarest,450),(Dobresta,379)]
# 10   | Pitesti        | [(Craiova,366),(Bucarest,418),(Dobresta,379)]
# 11   | Craiova        | [(Bucarest,418),(Dobresta,379)]
# 12   | Dobresta       | [(Bucarest,418)]
# 13   | Bucarest

# Ruta UCS: Arad -> Sibiu -> Rimnicu Vilcea -> Pitesti -> Bucarest (costo 418)


# ---------------------------------------
# b. Explorar el árbol de búsqueda con el método A* usando como heurística
# la distancia en línea recta. Indicar en cada nodo el número de expansión
# y el valor de g y h.

# A* utiliza f(n) = g(n) + h(n), donde hH(n) es la tabla de distancias
# en línea recta a Bucarest que se proporciona en la imagen.

# Paso | Nodo Expandido | Frontera
# 0    | -              | [(Arad,0,366,366)]
# 1    | Arad           | [(Zerind,75,374,449),(Sibiu,140,253,393),(Timisoara,118,329,447)]
# 2    | Sibiu          | [(Zerind,75,374,449),(Timisoara,118,329,447),(Fagaras,239,178,417),(Rimnicu,220,193,413)]
# 3    | Rimnicu Vilcea | [(Zerind,75,374,449),(Timisoara,118,329,447),(Fagaras,239,178,417),(Pitesti,317,98,415),(Craiova,366,160,526)]
# 4    | Pitesti        | [(Zerind,75,374,449),(Timisoara,118,329,447),(Fagaras,239,178,417),(Craiova,366,160,526),(Bucarest,418,0,418)]
# 5    | Fagaras        | [(Zerind,75,374,449),(Timisoara,118,329,447),(Craiova,366,160,526),(Bucarest,418,0,418)]
# 6    | Bucarest

# Ruta A* = Arad -> Sibiu -> Rimnicu Vilcea -> Pitesti (costo 418)

# Comparación: A* llegó a la misma solución óptima que UCS, pero explorando
# significativamente menos nodos. Mientras que UCS "vaga" por el mapa
# expandiendo ciudades lejanas como Lugoj o Mehadia porque están "cerca" en
# costo real, A* utiliza la heurística para enfocarse en la dirección
# correcta hacia Bucharest.