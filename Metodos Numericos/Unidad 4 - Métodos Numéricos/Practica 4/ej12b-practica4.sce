function [Q, R] = matricesQR(A)
    [m, n] = size(A);
    
    // Chequeamos si las columnas de A son lineal independientes.
    if rank(A) <> n then
        error("matricesQR: Las columnas de A no son lineal independientes");
    end
    
    // Inicializamos las matrices Q y R.
    Q = zeros(m, n);
    R = zeros(n, n);
    
    // Construcción de matrices.
    for k=1:n
        // Tomamos la columna k de A.
        v = A(:,k);
        
        // Ortogonalizamos el vector v con respecto a los anteriores.
        for j=1:k-1
            R(j, k) = Q(:, j)' * v; // Producto interno
            v = v - R(j, k)*Q(:,j); // Restamos la proyección de v sobre Q(:,j)
        end
        
        // Normalizamos el vector v y lo asignamos como k-esima columna de Q. 
        R(k, k) = norm(v);
        Q(:, k) = v / R(k,k)
    end
endfunction

function x = factQR(Q, R, b)
    n = size(R, 2);
    
    // Calculamos y = Q^t*b
    y = Q' * b;
    
    // Inicializamos el vector x.
    x = zeros(n, 1);
    
    // Sustitución regresiva para calcular R*x = Q^t*b
    for i=n:-1:1
        sumatoria = 0;
        for k=i+1:n
            sumatoria = sumatoria + R(i,k)*x(k);
        end
        
        x(i) = (y(i) - sumatoria)/R(i, i);
    end
endfunction

// Matriz del ejercicio.
A = [16 -12 8; -12 18 -6; 8 -6 8];
b = [76 -66 46]';

// Realizamos la factorización QR
[Q, R] = matricesQR(A);
disp(Q)
disp(R)

// Calculamos la solución del sistema
x = factQR(Q, R, b);

// Mostramos la solución
disp("Solución x:");
disp(x);

// Verificamos si A * x = b
disp("Verificación A * x:");
disp(A * x);
