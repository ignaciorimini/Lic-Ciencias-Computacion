function [x, iteraciones] = metodoGaussSeidel(A,b,x0,tol,maxIter)
    n = size(A, 1);     // Numero de filas de la matriz.
    x = x0;             // Vector inicial.
    iteraciones = 0;    // Contador de iteraciones.
    
    while iteraciones < maxIter
        xOld = x;       // Guardamos el valor anterior de x.
        
        // Iteramos sobre cada ecuación.
        for i=1:n
            suma = 0;
            
            // Suma de terminos anteriores a x_i, con valores nuevos.
            for j=1:i-1
                suma = suma + A(i,j)*x(j);
            end
            
            // Suma de términos posteriores a x_i, con valores viejos.
            for j=i+1:n
                suma = suma + A(i,j)*xOld(j);
            end
            
            x(i) = (b(i) - suma)/A(i,i);
        end
        
        iteraciones = iteraciones + 1;
        
        // Verificar tolerancia de error.
        if norm(x - xOld) < tol then
            return;
        end
    end
endfunction

function [x, a] = gausselimPivot(A,b)
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
    
    // Eliminación gaussiana con pivoteo parcial.
    for k=1:n-1
        // Encontrar el índice de la fila con el max valor absoluto en la columna k desde la fila k en adelante.
        // max devuelve el par [maxValor, indice], ignoramos el primero.
        // El índice obtenido por max() es relativo al subvector a(k:n, k), es decir, empieza en 1 en lugar de k. Por eso la segunda línea.
        [maxValor, max_row] = max(abs(a(k:n, k)));
        max_row = max_row + k - 1;
        
        // Intercambiar filas si es necesario para poner el mayor elemento
        if max_row <> k then
            temp = a(k,:);
            a(k, :) = a(max_row, :);
            a(max_row, :) = temp;
        end
        
        // Realizar eliminación gaussiana.
        for i=k+1:n
            mult = a(i, k)/a(k, k);
            a(i,k:n+1) = a(i,k:n+1) - mult*a(k,k:n+1);
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

// Sistema del ejercicio.
N = 100;
A = 8*eye(N,N) + 2*diag(ones(N-1,1),1) + 2*diag(ones(N-1,1),1) + diag(ones(N-3,1),3) + diag(ones(N-3,1),-3);
b= ones(N,1);
x0 = zeros(N,1);
tol = 1e-6;
maxIter = 100;

// MÉTOD0 DE GAUSS CON PIVOTEO PARCIAL: 0.1204264 segundos.
tic();
[x,a] = gausselimPivot(A,b);
t = toc();
// disp("Solución por Gauss Pivoteo Parcial: ");
// disp(x);
disp("Tiempo transcurrido: " + string(t) + " segundos");

// MÉTOD0 DE GAUSS-SEIDEL: 1.0508063 segundos.
tic();
[x, iter] = metodoGaussSeidel(A, b, x0, tol, maxIter);
t = toc();
// disp("Solución por Gauss-Seidel: ");
// disp(x);
disp("Tiempo transcurrido: " + string(t) + " segundos");

