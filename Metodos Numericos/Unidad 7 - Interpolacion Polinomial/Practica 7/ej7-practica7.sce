// MetodO de eliminación Gaussiana con pivoteo parcial.
// Notar que no se pide la hipótesis de que A tenga elementos distinto de ceros en la diagonal.
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

// Función que realiza un ajuste polinómico por el métod0 de Mínimos Cuadrados
// usando un polinomio de grado especificado.
// Los vectores x, y son vectores fila.
function [A,px,err] = minimosCuadrados(x,y,grado)    
    // Verificar que tengan mismo tamaño.
    n = length(x);
    if n <> length(y) then
        error("minimosCuadrados: x e y deben tener misma cantidad de elementos");
    end
    
    // Construir matriz A.
    A = ones(n,grado+1);
    
    for j=2:grado+1
        A(:,j) = (x').^(j-1);
    end
    
    // Resolver sistema At*A*x = At*y
    [coeffs, Aaum] = gausselimPivot(A'*A, A'*y');
    
    // Construir polinomio con los coeficientes obtenidos.
    px = poly(coeffs, "x", "coeff");
    
    // Cálculo del error.
    err = norm(A*coeffs - y');
endfunction

// Ejercicio.
x = [0, 0.15, 0.31, 0.5, 0.6, 0.75];
y = [1, 1.004, 1.031, 1.117, 1.223, 1.422];

// Grado 1
[A, px, err] = minimosCuadrados(x, y, 1);
disp("Coeficientes del polinomio ajustado:", px);
disp("Error del ajuste:", err);

// Grado 2
[A, px, err] = minimosCuadrados(x, y, 2);
disp("Coeficientes del polinomio ajustado:", px);
disp("Error del ajuste:", err);

// Grado 3: mínimo error máximo
[A, px, err] = minimosCuadrados(x, y, 3);
disp("Coeficientes del polinomio ajustado:", px);
disp("Error del ajuste:", err);
