// Matriz del ejercicio 1a.
A = [1 0 0; -1 0 1; -1 -1 2];
autovalores = spec(A);  // Devuelve los autovalores de A.
disp("Autovalores de Aa: ", autovalores);

// Matriz del ejercicio 1b.
A = [1 0 0; -0.1 0 0.1; -0.1 -0.1 2];
autovalores = spec(A);
disp("Autovalores de Ab: ", autovalores);

// Matriz del ejercicio 1c.
A = [1 0 0; -0.25 0 0.25; -0.25 -0.25 2];
autovalores = spec(A);
disp("Autovalores de Ac: ", autovalores);

// Matriz del ejercicio 1d.
A = [4 -1 0; -1 4 -1; -1 -1 4];
autovalores = spec(A);
disp("Autovalores de Ad: ", autovalores);

// Matriz del ejercicio 1e.
A = [3 2 1; 2 3 0; 1 0 3];
autovalores = spec(A);
disp("Autovalores de Ae: ", autovalores);

// Matriz del ejercicio 1f.
A = [4.75 2.25 -0.25; 2.25 4.75 1.25; -0.25 1.25 4.75];
autovalores = spec(A);
disp("Autovalores de Af: ", autovalores);

