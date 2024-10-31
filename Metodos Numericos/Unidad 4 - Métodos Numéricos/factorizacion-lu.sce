// Toma la matriz A de coeficientes y devuelve la matriz U que surge de aplicar eliminación gaussiana a A, junto con la matriz L.
// Supone a la matriz A sin ceros en su diagonal principal.
function [L, U] = matricesLU(U)
    [n, m] = size(U);
    
    if n <> m then
        error("matricesLU: la matriz debe ser cuadrada");
    end
    
    // Inicializamos L como una identidad y U = matriz A argumento.
    L = eye(n,n);
    
    // Eliminación gaussiana y construcción de L.
    for k=1:n-1
        for i=k+1:n
            L(i, k) = U(i, k)/U(k, k); // Multiplicador
            U(i, k:n) = U(i, k:n) - L(i, k)*U(k, k:n);
        end
    end
endfunction


// Toma una matriz L triangular inferior y U triangular superior y calcula A*x = b usando la factorización LU: Ly = b -> Ux = y
function x = factLU(L,U,b)
    n = length(b);
    
    // Inicializamos los vectores columna y, x.
    y = zeros(n, 1);
    x = zeros(n, 1);
    
    // Sustitución progresiva (hacia adelante) para L*y = b.
    for i=1:n
        sumatoria = 0;
        for k=1:i-1
            sumatoria = sumatoria + L(i,k)*y(k);
        end
        
        y(i) = b(i) - sumatoria;
    end
    
    // Sustitución regresiva (hacia atrás) para U*x = y.
    for i=n:-1:1
        sumatoria = 0;
        for k=i+1:n
            sumatoria = sumatoria + U(i,k)*x(k);
        end
        
        x(i) = (y(i) - sumatoria)/U(i,i);
    end
endfunction


// Ejemplo de uso. Respuesta: 0.5, 0, 0
A = [2 3 1; 4 7 2; 6 18 5];
b = [1, 2, 3]';

[L, U] = matricesLU(A);
x = factLU(L, U, b);
disp(x);
