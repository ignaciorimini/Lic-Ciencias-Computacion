// Supone que la matriz U no tiene ceros en la diagonal principal.
function [L, U] = matricesDoolittle(U)
    [n, m] = size(U);
    
    if n <> m then
        error("matricesDoolittle: la matriz debe ser cuadrada");
    end
    
    // Inicializamos L como matriz identidad.
    L = eye(n, n);
    
    for i = 1:n
        // Calcular los elementos de la matriz U (parte superior)
        for j = i:n
            factorSum = sum(L(i, 1:i-1) .* U(1:i-1, j)');
            U(i, j) = U(i, j) - factorSum;
        end
        
        // Calcular los elementos de la matriz L (parte inferior)
        for j = i+1:n
            factorSum = sum(L(j, 1:i-1) .* U(1:i-1, i)');
            L(j, i) = (U(j, i) - factorSum) / U(i, i);
            
            // Hacer ceros debajo de la diagonal en U
            U(j, i) = 0;
        end
    end
endfunction

function x = calcDoolittle(L, U, b)
    n = length(b);
    
    // Inicializamos los vectores columna.
    y = zeros(n, 1);
    x = zeros(n, 1);
    
    // Sustitución progresiva para L*y = b.
    for i=1:n
        sumatoria = 0;
        for k=1:i-1
            sumatoria = sumatoria + L(i,k)*y(k);
        end
        
        y(i) = b(i) - sumatoria;
    end
    
    // Sustitución regresiva para U*x = y.
    for i=n:-1:1
        sumatoria = 0;
        for k=i+1:n
            sumatoria = sumatoria + U(i, k)*x(k);
        end
        
        x(i) = (y(i) - sumatoria)/U(i, i)
    end
endfunction

// Matriz del ejercicio.
A = [1 2 3 4; 1 4 9 16; 1 8 27 64; 1 16 81 256];
b = [2 10 44 190]';

// Factorización LU con Doolittle
[L, U] = matricesDoolittle(A);

// Resolver el sistema usando calcDoolittle
x = calcDoolittle(L, U, b);

disp("Solución x:");
disp(x);
disp(A*x);
