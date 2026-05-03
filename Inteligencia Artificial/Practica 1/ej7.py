# 7. Un robot se mueve en una grilla de nxn. En un determinado momento,
# el robot se encontrará en una celda (determinada por su posición X
# indicando la fila y la posición Y indicando la columna), con una
# orientación la cual puede tener los siguientes valores:
# - N: Norte
# - S: Sur
# - E: Este
# - O: Oeste

# Los movimientos que puede realizar el robot en esta grilla son:

# 1. Avanzar: el robot se dirige a la celda contigua (costo 2). Esta celda
# depende de la orientación del robot. La tabla resume la ubicación del 
# robot después de ejecutar esta acción:

# Orientación | Próxima Ubicación
# N           | X-1, Y
# S           | X+1, Y
# E           | X, Y+1
# O           | X, Y-1

# 2. Girar: el robot gira 90° en el sentido de las agujas del reloj.
# El siguiente gráfico muestra la rotación del robot (costo 1):
# O -> N -> E -> S -> O

# El robot tendrá una posición inicial en la grilla y se le solicitará
# que llegue a alguna celda destino. Además se especificarán un grupo
# de celdas prohibidas. Por lo tanto, el robot tendrá que resolver como
# problema determinar el camino a seguir en la grilla para alcanzar el 
# destino final, sin pasar por las celdas prohibidas.

# ---------------------------------------
# a. Representar el problema de desplazamiento del robot mediante un
# espacio de estados.

# ESTADOS: Una tupla (X,Y,O) donde:
# - X ϵ {1, ... n} representa la fila
# - Y ϵ {1, ... n} representa la columna
# - O ϵ {N,S,E,O} representa la orientación actual.

# ESTADO INICIAL: (1,1,N)

# ESTADO META: (3,4,Cualquiera), es decir, (3,4) sin importar la orientación.

# ACCIONES:
# - Avanzar: si la celda destino no es prohibida ni está fuera de grilla, costo = 2.
# - Girar: cambiar la orientación (N -> E -> S -> O -> N), costo = 1.

# RESTRICCIONES: no se pueden expandir estados que sean celdas prohibidas
# (1,2), (1,4), (2,2).


# ---------------------------------------
# b. Definir una función heurística y aplicar búsqueda A* para hallar
# el camino de menor costo entre (1,1) si el robot comienza con orientación
# N y (3,4) cualquier orientación, en una grilla de (5x5), teniendo como
# celdas prohibidas: (1,2), (1,4) y (2,2).

# Una heurística adecuada y admisible para este problema es la Distancia
# de Manhattan, pero multiplicada por el costo mínimo de movimiento
# (que es 2 por cada celda de distancia).

# h(n) = 2 x (|Xn - Xmeta| + |Yn - Ymeta|)

# Esta heurística es admisible porque asume que el robot ya está orientado
# correctamente y no necesita girar (costo 0 por giros), por lo que nunca
# sobreestimará el costo real.