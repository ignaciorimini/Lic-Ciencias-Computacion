# 3. Representar el problema de los MISIONEROS Y CANÍBALES descripto 
# a continuación, mediante un espacio de estados y aplicar búsqueda primero
# a lo ancho para resolver este problema. Encontrar la solución de menos pasos.

# MISIONEROS Y CANÍBALES
# Tres misioneros y tres caníbales se encuentran en una orilla,
# junto a una canoa en la que pueden cruzar 1 o 2 personas. Es decir, el bote
# nunca puede estar vacío. Hay que encontrar la forma de pasarlos 
# a todos a la otra orilla pero teniendo en cuenta que en ningún momento 
# el número de misioneros sea menor que el de los caníbales.

# ---------------------------------------
# REPRESENTACIÓN DEL ESPACIO DE ESTADOS

# Definimos un estado como una terna (M,C,B) donde:
# M: número de misioneros en la orilla de origen (izquierda) {0,1,2,3}.
# C: número de canibales en la orilla de origen (izquierda) {0,1,2,3}.
# B: posición del bote (1 si está en la orilla izquierda, 0 derecha).

# ESTADO INICIAL: (3,3,1)

# ESTADO META: (0,0,0)

# RESTRICCIONES (Estados Válidos)
# En ninguna de las dos orillas el número de misioneros puede ser menor
# que el de los caníbales (salvo que el número de misioneros sea 0).
# - Orilla izquierda: M >= C (si M > 0) y M <= 3, C <= 3.
# - Orilla derecha: (3 - M) >= (3 - C) (si 3 - M > 0).

# ---------------------------------------
# APLICACIÓN DE BÚSQUEDA A LO ANCHO (BFS)

# Paso | Nodo Expandido | Frontera
# 0    | -              | [(3,3,1)]
# 1    | (3,3,1)        | [(3,1,0), (3,2,0), (2,2,0)]
# 2    | (3,1,0)        | [(3,2,0), (2,2,0), (3,2,1)]
# 3    | (3,2,0)        | [(2,2,0), (3,2,1), (3,3,1)] ult ya expandido
# 4    | (2,2,0)        | [(3,2,1), (3,2,1)] ult ya en frontera
# 5    | (3,2,1)        | [(3,0,0)]
# 6    | (3,0,0)        | [(3,1,1)]
# 7    | (3,1,1)        | [(1,1,0)]
# 8    | (1,1,0)        | [(2,2,1)]
# 9    | (2,2,1)        | [(0,2,0)]
# 10   | (0,2,0)        | [(0,3,1)]
# 11   | (0,3,1)        | [(0,1,0)]
# 12   | (0,1,0)        | [(0,2,1), (1,1,1)]
# 13   | (0,2,1)        | [(1,1,1), (0,0,0)]
# 14   | (0,0,0)        | Meta alcanzada

# Observaciones:
# - Al expandir (3,3,1) los estados (2,3,0) y (1,3,0) se descartan por ser inválidos
# - Del paso 5 al 11 la frontera se mantiene pequeña. Esto es por muchas acciones
# posibles resultan en estados prohibidos o regresan a un estado anterior.
# - En el paso 12 suegen dos caminos válidos. Al expandir primero el primero
# en el paso 13, encontramos la meta.
# - Esta secuencia es la solución óptima, que es un camino de 11 aristas.
