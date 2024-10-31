// Toma una matriz A cuadrada de coeficientes y un vector columna b y resuelve el sistema de ecuaciones lineales Ax = b.
// Supone que A no tiene elementos nulos en la diagonal.
// Devuelve el vector de soluciones y la matriz ampliada resultante (triangular superior) luego de aplicar la eliminación gaussiana.
function [x, a] = gausselim(A,b)
    [nA, mA] = size(A);
    [nb, mb] = size(b);
    
    if nA <> mA then
        error("gausselim: matriz no es cuadrada");
    elseif mA <> nb then
        error("gausselim: no coinciden las dimensiones de A y b");
    end
    
    // Declaramos matriz aumentada.
    a = [A b];
    n = nA;
    
    // Eliminación gaussiana.
    for k=1:n-1
        for i=k+1:n
            mult = a(i,k)/a(k,k);
            a(i,k) = 0;
            a(i,k+1:n+1) = a(i,k+1:n+1) - mult*a(k,k+1:n+1);
        end
    end
    
    // Inicializar el vector solución
    x = zeros(n, 1);

    // Sustitución regresiva.
    x(n) = a(n, n+1)/a(n, n);
    for i = n-1:-1:1
        sumk = 0;
        for k=i+1:n
            sumk = sumk + a(i,k)*x(k);
        end
        
        x(i) = (a(i, n+1) - sumk)/a(i,i);
    end
endfunction

// Ejemplo de aplicación. Solución x1=1, x2=1, x3=1
A = [3 -2 -1; 6 -2 2; -9 7 1];
b = [0 6 -1]';

A = [1 2 1; 2 2 3; -1 -3 0];
b = [0 3 2]';

[x, a] = gausselim(A, b);
disp(x);    // Vector de soluciones.
disp(a);    // Matriz ampliada luego de Gauss
disp(A*x)   // Verificación Ax = b

// Ejemplo de aplicación con error por no usar pivoteo parcial.
A2 = [0 2 3; 2 0 3; 8 16 -1]
b2 = [7 13 -3]'
[x2,a2] = gausselim(A2,b2)
disp(x2)
disp(a2)
