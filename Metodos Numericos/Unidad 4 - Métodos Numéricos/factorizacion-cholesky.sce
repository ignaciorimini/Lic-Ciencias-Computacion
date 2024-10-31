// Retorna ind = 0 si hubo un error e ind = 1 si se pudo factorizar la matriz.
function [U, ind] = matrizCholesky(A)
    [n, m] = size(A);
    eps = 1e-8;
    
    if n <> m then
        error("factCholesky: la matriz debe ser cuadrada");
    end
    
    // Inicializamos la matriz U como la matriz nula de tamaño nxn.
    U = zeros(n, n);
    
    // Construcción de la matriz U utilizando Cholesky.
    for k=1:n
        // Cálculo del elemento diagonal U(k, k).
        sumatoria_diag = sum(U(1:k-1, k).^2);
        t = A(k, k) - sumatoria_diag;
        
        if t <= eps then
            printf("Matriz no definida positiva.\n");
            ind = 0;
            return;
        end
        
        // Si t es un valor adecuado, calculamos la raíz y se la asignamos al elemento diagonal U(k, k).
        U(k, k) = sqrt(t);
        
        // Cálculo de elementos fuera de la diagonal.
        for j=k+1:n
            factorSumatoria = sum(U(1:k-1, k) .* U(1:k-1, j));
            U(k, j) = (A(k, j) - factorSumatoria) / U(k, k);
        end
    end
    ind = 1;
endfunction

function x = factCholesky(U, b)
    // Inicialización de variables.
    n = length(b);
    y = zeros(n, 1);
    x = zeros(n, 1);
    
    // Sustitución progresiva para calcular U^t*y = b.
    for i=1:n
        sumatoria = 0;
        for k=1:i-1
            // Usamos U(k, i) para referirnos a U^t.
            sumatoria = sumatoria + U(k,i)*y(k);
        end
        
        y(i) = (b(i) - sumatoria)/U(i,i);
    end
    
    // Sustitución regresiva para calcular U*x = y.
    for i=n:-1:1
        sumatoria = 0;
        for k=i+1:n
            sumatoria = sumatoria + U(i,k)*x(k);
        end
        
        x(i) = (y(i) - sumatoria)/U(i,i);
    end
endfunction

// Definimos una matriz definida positiva y simétrica
A = [4 12 -16; 12 37 -43; -16 -43 98];
b = [11; 70; 17];

// Factorización de Cholesky
[U, ind] = matrizCholesky(A);

if ind == 1 then
    // Resolvemos el sistema usando la factorización de Cholesky
    x = factCholesky(U, b);
    disp("Solución x:");
    disp(x);
    disp(A*x);
end

A = [16 -12 8 -16; -12 18 -6 9; 8 -6 5 -10; -16 9 -10 46];
[U, ind] = matrizCholesky(A)
disp(U);

