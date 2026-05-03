# 1. Encontrar una representación adecuada del espacio de estado
# para los siguientes problemas:

# ---------------------------------------
# a. El juego de los 8 números (8-puzzle)
# Dada una configuración de ocho números en un tablero de 3x3
# llevarlo mediante el desplazamiento de números a la situación final:
# 1 2 3
# 8 - 4
# 7 6 5

# ESTADOS: Podemos representar al espacio de estados como matrices 3x3,
# donde cada posición de la matriz representa la ficha que hay
# en dicha coordenada. 

# ESTADO INICIAL: cualquier estado puede ser designado como estado inicial

# ACCIONES: mover el espacio en blanco hacia la Izquierda, Derecha,
# Arriba o Abajo. En las esquinas se reducen los movimientos.

# MODELO DE TRANSICIÓN: el estado resultante al aplicar una acción
# es intercambiar de lugar el espacio en blanco con la ficha que
# se encontraba en la posición donde se mueve.

# ESTADO META: el tablero dado en la consigna.

# COSTE DE LA ACCIÓN: cada acción cuesta 1.


# ---------------------------------------
# b. La torre de Hanoi
# Hay 64 discos de diámetro decreciente en un poste y hay que pasarlos
# a otro poste, utilizando un tercero para los pasos intermedios.
# Sólo puede moverse un disco a la vez, siempre deben estar en algún
# poste y no se puede colocar un disco sobre otro de menor tamaño.

# ESTADOS: matrices 64x3, con valores naturales del 0 al 1.
# Las posiciones con valor 0 especifican que no hay un disco allí.
# Las posiciones con valores > 0 indican que hay un disco, e indican su diámetro.

# ESTADO INICIAL: la matriz 64x3:
# 01 00 00
# 02 00 00
# .. .. ..
# 64 00 00

# ACCIONES: 
# - Se intercambia un valor mayor a 0 por un valor 0.
# - No puede haber un valor en una fila superior que un valor menor qué el. 
# Es decir, no puede estar el valor 2 en la fila 2 si en las filas 3, 4,
# 5 tiene valores 0 o menores a sí mismo.

# EJEMPLO DE TRANSICION:
# 01 00 00  |  00 00 00  |  00 00 00
# 02 00 00  |  02 00 00  |  00 00 00
# .. .. ..  |  .. .. ..  |  .. .. ..
# 64 00 00  |  64 01 00  |  64 01 02

# ESTADO META:
# 00 01 00
# 00 02 00
# .. .. ..
# 00 64 00

# COSTE DE ACCIÓN: cada acción cuesta 1.


# ---------------------------------------
# c. Cuadrado Latino.
# Consiste en llenar un cuadrado de 3x3 con un elemento del conjunto {1,2,3}
# de forma tal que en cada fila y columna no haya elementos repetidos.

# ESTADOS: matrices 3x3 donde en cada posición hay un valor
# del conjunto {0,1,2,3}, donde un valor 0 indica que no hay ningún elemento.

# ESTADO INICIAL: la matriz 3x3
# 0 0 0
# 0 0 0
# 0 0 0

# ACCIONES:
# - Colocar un elemento del conjunto {1,2,3} en alguna posición de la matriz.
# - No puede haber elementos del conjunto repetidos en una misma fila.
# - No puede haber elementos del conjunto repetidos en una misma columna.

# ESTADOS META: matrices 3x3 sin elementos 0, con todos elementos
# del conjunto {1,2,3} donde no se repiten en una misma fila y columna.

# COSTE DE ACCIÓN: cada acción cuesta 1.