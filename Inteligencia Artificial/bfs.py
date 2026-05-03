from collections import deque

class Node:
    """Representa un nodo en el árbol de búsqueda."""
    def __init__(self, state, parent=None, action=None, path_cost=0):
        self.state = state
        self.parent = parent
        self.action = action
        self.path_cost = path_cost

    def __repr__(self):
        return f"<Node {self.state}>"

    def path(self):
        """Retorna la lista de estados desde la raíz hasta este nodo."""
        node, path_back = self, []
        while node:
            path_back.append(node.state)
            node = node.parent
        return list(reversed(path_back))

class GraphProblem:
    """Define el problema de búsqueda en un grafo."""
    def __init__(self, initial, goal, graph):
        self.initial = initial
        self.goal = goal
        self.graph = graph

    def is_goal(self, state):
        return state == self.goal

    def expand(self, node):
        """Genera los nodos hijos a partir de un nodo padre."""
        s = node.state
        for action, next_state in self.graph.get(s, {}).items():
            cost = node.path_cost + 1  # En BFS asumimos costo unitario
            yield Node(state=next_state, parent=node, action=action, path_cost=cost)

def breadth_first_search(problem):
    """
    Implementación de BFS con Early Goal Test y reached set.
    """
    # Creamos el nodo raíz
    node = Node(state=problem.initial)
    
    # Early Goal Test: ¿Es el estado inicial la meta?
    if problem.is_goal(node.state):
        return node
    
    # frontier: Cola FIFO (usamos deque para eficiencia O(1) en popleft)
    frontier = deque([node])
    
    # reached: Conjunto de estados ya explorados
    reached = {problem.initial}
    
    while frontier:
        # Extraemos el nodo más viejo (el más superficial)
        node = frontier.popleft()
        
        # Expandimos los hijos
        for child in problem.expand(node):
            s = child.state
            
            # Early Goal Test: Comprobar meta al generar el hijo
            if problem.is_goal(s):
                return child
            
            # Si el estado es nuevo, lo marcamos y lo añadimos a la cola
            if s not in reached:
                reached.add(s)
                frontier.append(child)
                
    return None # Failure

# --- Ejemplo de uso (Mapa de Rumania simplificado) ---
romania_map = {
    'Arad': {'to Zerind': 'Zerind', 'to Sibiu': 'Sibiu', 'to Timisoara': 'Timisoara'},
    'Zerind': {'to Oradea': 'Oradea'},
    'Sibiu': {'to Fagaras': 'Fagaras', 'to Rimnicu': 'Rimnicu Vilcea'},
    'Fagaras': {'to Bucharest': 'Bucharest'},
    'Rimnicu Vilcea': {'to Pitesti': 'Pitesti'},
    'Pitesti': {'to Bucharest': 'Bucharest'}
}

# Definimos el problema: Ir de Arad a Bucharest
problem = GraphProblem(initial='Arad', goal='Bucharest', graph=romania_map)

# Ejecutamos la búsqueda
solution_node = breadth_first_search(problem)

if solution_node:
    print(f"¡Solución encontrada! Camino: {solution_node.path()}")
    print(f"Costo total (pasos): {solution_node.path_cost}")
else:
    print("No se encontró solución.")