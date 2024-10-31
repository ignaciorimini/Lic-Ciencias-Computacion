function [x,iteraciones] = metodoGaussSeidelTridiagonal(A,b,x0,tol,maxIter)
    n = size(A,1);
    x = x0;
    iteraciones = 0;
    
    while iteraciones < maxIter
        xOld = x;
        
        // Primer valor de x.
        suma = A(1,2)*xOld(2);
        x(1) = (b(1) - suma)/A(1,1);
        
        // Iteraciones para los próximos valores de x.
        for i=2:n-1
            suma = A(i,i-1)*x(i-1) + A(i,i+1)*xOld(i+1);
            x(i) = (b(i) - suma)/A(i,i);
        end
        
        // Último valor de x.
        suma = A(n,1)*x(1) + A(n,n-1)*x(n-1);
        x(n) = (b(n) - suma)/A(n,n);
        
        iteraciones = iteraciones + 1;
        
        // Verificar tolerancia del error.
        if norm(x - xOld) < tol then
            return;
        end
    end
endfunction

function [A,b] = construirSistema(n,elemDiag)
    // Diagonal de A.
    A = eye(n,n);
    A = elemDiag * A;
    
    // Diagonal arriba de la primera fila.
    A(1,2) = -1;
    
    // Iteración para diagonal arriba y abajo.
    for i=2:n-1
        A(i,i-1) = -1;
        A(i,i+1) = -1;
    end
    
    // Ultima fila.
    A(n,1) = 1;
    A(n,n-1) = -1;
    
    // Construir vector b.
    b = zeros(n,1);
    b(n) = 1;
endfunction

function M = matrizIteracion(A)
    n = size(A,1);
    N = tril(A);
    M = eye(n,n) - inv(N)*A;
endfunction

// Matriz del ejercicio.
[A,b] = construirSistema(20,2);
n = size(A,1);
x0 = zeros(n,1);
tol = 1e-8;
maxIter = 1000;

[x, iteraciones] = metodoGaussSeidelTridiagonal(A,b,x0,tol,maxIter);
disp("Iteraciones: " + string(iteraciones));
disp("Solución: ", x);

M = matrizIteracion(A);
radioEspectral = max(abs(spec(M)));
disp("Radio espectral: " + string(radioEspectral));
