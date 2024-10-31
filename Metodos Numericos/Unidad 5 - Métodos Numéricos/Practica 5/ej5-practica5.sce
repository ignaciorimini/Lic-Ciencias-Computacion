function [x, iteraciones] = metodoSOR(A,b,x0,w,tol,maxIter)
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
            
            // Suma de terminos posteriores a x_i, con valores viejos.
            for j=i+1:n
                suma = suma + A(i,j)*xOld(j);
            end
            
            // Actualización de x(i) usando SOR
            x(i) = (1-w)*xOld(i) + (w/A(i,i)) * (b(i) - suma);
        end 
        
        iteraciones = iteraciones + 1;
        
        // Verificar tolerancia de error.
        if norm(x - xOld) < tol then
            return;
        end
    end
endfunction

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

function w = omegaOptimo(A)
    n = size(A,1);
    Tj = eye(n,n) - diag(1./diag(A))*A;
    autovalores = spec(Tj);
    radioEspectral = max(abs(autovalores));
    w = 2/(1 + sqrt(1 - radioEspectral^2));
endfunction

// Sistema del ejercicio.
A = [4 3 0; 3 4 -1; 0 -1 4];
b = [24; 30; -24];
x0 = [0; 0; 0];
tol = 1e-7;
maxIter = 100;
w = omegaOptimo(A);

// MÉTOD0 DE GAUSS-SEIDEL: 36 iteraciones.
[x, iter] = metodoGaussSeidel(A, b, x0, tol, maxIter);
disp("Solución por Gauss-Seidel: ");
disp(x);
disp("Número de iteraciones: " + string(iter));

// MÉTOD0 DE SOR: 16 iteraciones.
[x, iter] = metodoSOR(A, b, x0, w, tol, maxIter);
disp("Solución por SOR con w = " + string(w) + ": ");
disp(x);
disp("Número de iteraciones: " + string(iter));
