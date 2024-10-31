function [L, U, P] = matricesLUP(U)
    [n, m] = size(U);
    
    if n <> m then
        error("matricesLUP: la matriz debe ser cuadrada");
    end
    
    // Inicializamos L como matriz nula, P como la identidad y U = A matriz argumento.
    L = zeros(n, n);
    P = eye(n, n);
    
    // Eliminación gaussiana con pivoteo parcial utilizando matriz P de permutación.
    for k=1:n-1                         // Iterador de columnas
        [max_valor, max_row] = max(abs(U(k:n, k)));
        max_row = max_row + k - 1;
        
        if max_row <> k then
            // Intercambiar filas en U.
            temp = U(k, :);
            U(k, :) = U(max_row, :);
            U(max_row, :) = temp;
            
            // Intercambiar filas en P.
            temp = P(k, :);
            P(k, :) = P(max_row, :);
            P(max_row, :) = temp;
            
            // Intercambiar filas en L (hasta columna k-1).
            if k > 1 then
                temp = L(k, 1:k-1);
                L(k, 1:k-1) = L(max_row, 1:k-1);
                L(max_row, 1:k-1) = temp;
            end
        end
        
        for i=k+1:n                     // Iterador de fila
            L(i, k) = U(i,k)/U(k, k);   // Multiplicador
            U(i, k:n) = U(i, k:n) - (L(i, k)*U(k, k:n));
        end
    end
    
    // Completar la diagonal de L con 1s
    for i = 1:n
        L(i, i) = 1;
    end
endfunction


function x = factLUP(L,U,P,b)
    n = length(b);
    
    // Inicializamos los vectores columnas para los sistemas.
    x = zeros(n, 1);
    y = zeros(n, 1);
    
    // Permutamos el vector b utilizando la matriz P.
    b2 = P*b;
    
    // Sustitución progresiva para resolver L*y = b2.
    for i=1:n
        sumatoria = 0;
        for k=1:i-1
            sumatoria = sumatoria + L(i,k)*y(k);
        end
        
        y(i) = b2(i) - sumatoria;
    end
    
    // Sustitución regresiva para resolver U*x=y.
    for i=n:-1:1
        sumatoria = 0;
        for k=i+1:n
            sumatoria = sumatoria + U(i,k)*x(k);
        end
        
        x(i) = (y(i) - sumatoria)/U(i,i);
    end
endfunction

// Ejemplo de matriz A y vector b
A = [1 2 -2 1; 4 5 -7 6; 5 25 -15 -3; 6 -12 -6 22];
b = [1 2 0 1]';

// Calcular L, U y P usando matricesLUP
[L, U, P] = matricesLUP(A);

// Resolver el sistema (a) con factLUP
x = factLUP(L, U, P, b);
disp("Solución x:");
disp(x);
disp(A*x);

// Resolver el sistema (b) con factLUP
b = [2 2 1 0]';
x = factLUP(L, U, P, b);
disp("Solución x:");
disp(x);
disp(A*x);

