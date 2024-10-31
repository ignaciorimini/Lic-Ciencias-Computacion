function [x,a] = gaussElimTriDiag(A,b)
    [nA,mA] = size(A);
    [nB,mB] = size(b);
    
    if nA <> mA then
        error("gaussElimTriDiag: la matriz no es cuadrada");
    end
    
    if mA <> nB then
        error("gaussElimTriDiag: no coinciden las dimensiones de A y b");
    end
    
    // Declaramos matriz aumentada.
    a = [A b];
    
    // Eliminación gaussiana.
    n = nA;
    for k=1:n-1
        if a(k,k) == 0 then
            error("gaussElimTriDiag: la matriz es singular");
        end
        
        mult = a(k+1,k)/a(k,k);     // Multiplicador.
        a(k+1,k) = 0;               // Ponemos cero debajo de diagonal.
        a(k+1,k+1) = a(k+1,k+1) - mult*a(k,k+1);
        a(k+1,n+1) = a(k+1,n+1) - mult*a(k,n+1);
    end
    
    // Sustitución regresiva.
    x = zeros(nA, 1);
    x(n) = a(n,n+1)/a(n,n);
    for k=n-1:-1:1
        sumatoria = a(k,k+1)*x(k+1);
        x(k)= (a(k,n+1) - sumatoria)/a(k,k);
    end
endfunction

// Matrices de ejemplo tridiagonales.
A = [1 1 0 0; 1 1 1 0 ; 0 1 1 1 ; 0 0 1 1]; // Matriz singular.
b = [2;3;3;2];
//[x, a] = gaussElimTriDiag(A,b);
//disp(x);

A = [2 2 0 0; 5 2 2 0; 0 5 2 2; 0 0 5 2]
b = [6;15;24;23]
[x, a] = gaussElimTriDiag(A,b);
disp(x);

A = [2 1 0 0;1 2 1 0;0 1 2 1;0 0 1 2]
b = [1;1;1;1]
[x, a] = gaussElimTriDiag(A,b);
disp(x);
