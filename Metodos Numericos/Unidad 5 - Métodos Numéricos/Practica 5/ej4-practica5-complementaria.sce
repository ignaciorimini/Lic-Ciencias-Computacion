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

function factLUIter(L,U,b)
    disp("Iteración 0:", b);
    for i=1:4
        x = factLU(L,U,b);
        disp("Iteración " + string(i) + ":", x);
        b = x;
    end    
endfunction

function A = matrizDadoEscalar(c)
    // Construcción de la diagonal de A.
    A = eye(5,5);
    A = (1 + 2*c) * A;
    
    // Construcción de elementos primera y última fila.
    A(1,2) = -c
    A(5,4) = -c
    
    // Construcción general.
    for i=2:4
        A(i,i-1) = -c;
        A(i,i+1) = -c
    end
endfunction

// Matrices del ejercicio.
A = matrizDadoEscalar(1);
[L,U] = matricesLU(A);
b = [10 12 12 12 10]';
factLUIter(L,U,b);

// disp("Matriz A: ", A);
// disp("Matriz L: ", L);
// disp("Matriz U:", U);
// disp("Verif L*U:", L*U);
