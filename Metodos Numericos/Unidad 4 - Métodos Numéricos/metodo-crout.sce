function [L, U] = matricesCrout(A)
    [n, m] = size(A);
    
    if n <> m then
        error("matricesCrout: la matriz debe ser cuadrada");
    end
    
    // Inicializar matrices.
    U = eye(n, n);
    L = zeros(n, n);
    
    // Construir matrices.
    for j=1:n
        // Calcular elementos de L.
        for i=j:n
            factorSuma = L(i, 1:j-1) .* U(1:j-1, j)';
            factorSuma = sum(factorSuma);
            L(i, j) = A(i, j) - factorSuma;
        end
        
        // Calcular elementos de U.
        for k=j+1:n
            factorSuma = L(j, 1:j-1) .* U(1:j-1, k);
            factorSuma = sum(factorSuma);
            U(j, k) = (A(j, k) - factorSuma)/L(j, j);
        end
    end
endfunction

function x = calcCrout(L, U, b)
    n = length(b);
    
    // Inicializamos los vectores columnas.
    y = zeros(n, 1);
    x = zeros(n, 1);
    
    // Sustitución progresiva para L*y=b
    for i=1:n
        sumatoria = 0;
        for k=1:i-1
            sumatoria = sumatoria + L(i, k)*y(k);
        end    
        
        y(i) = (b(i) - sumatoria)/L(i, i);
    end
    
    // Sustitución regresiva para U*x=y
    for i=n:-1:1
        sumatoria = 0;
        for k=i+1:n
            sumatoria = sumatoria + U(i,k)*x(k);
        end
        
        x(i) = y(i) - sumatoria;
    end
endfunction

// Ejemplo con una matriz A y un vector b
A = [4 -2 1; 20 -7 12; -8 13 17];
b = [11; 70; 17];

// Factorizamos A usando Crout
[L, U] = matricesCrout(A);

// Calculamos la solución del sistema Ax = b usando calcCrout
x = calcCrout(L, U, b);

disp("Solución x:");
disp(x);
disp(A*x);
