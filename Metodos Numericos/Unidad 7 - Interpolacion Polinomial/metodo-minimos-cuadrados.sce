// MetodO de eliminación Gaussiana con pivoteo parcial.
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

// Funcion que recibe un vector x = [x0, x1, ... xn] de nodos, el grado n (columnas)
// y construye la matriz de Vandermonde.
function A = matrizVandermonde(x,n)
    m = length(x);
    A = ones(m,n);
    
    for j=2:n
        A(:,j) = (x').^(j-1);
    end
endfunction

// Función que realiza un ajuste polinómico por el métod0 de Mínimos Cuadrados
// usando un polinomio de grado especificado.
// Los vectores x e y son vectores fila.
function [A,px,err] = minimosCuadrados(x,y,grado)    
    // Verificar que tengan mismo tamaño.
    n = length(x);
    if n <> length(y) then
        error("minimosCuadrados: x e y deben tener misma cantidad de elementos");
    end
    
    // Construir matriz A de Vandermonde.
    A = matrizVandermonde(x,grado+1)
    
    // Resolver sistema At*A*x = At*y
    [coeffs, Aaum] = gausselimPivot(A'*A, A'*y');
    
    // Construir polinomio con los coeficientes obtenidos.
    px = poly(coeffs, "x", "coeff");
    
    // Cálculo del error.
    err = norm(A*coeffs - y');
endfunction


// Función para graficar el polinomio interpolante.
function graficarPolinomio(p, xNodos, yNodos, rangoX)
    // Evaluar el polinomio p en el rango dado
    valoresX = linspace(rangoX(1), rangoX(2), 100);
    valoresY = horner(p, valoresX);
    
    // Graficar la curva del polinomio y los puntos.
    plot(xNodos,yNodos,'.');
    plot(valoresX, valoresY, "r", "LineWidth", 2);
    xgrid();
    
    // Títulos y etiquetas
    legend(["Datos originales", "Polinomio ajustado"]);
    title("Ajuste por Mínimos Cuadrados");
    xlabel("x");
    ylabel("y");
endfunction

// ______________________________________________
// Ejemplo de prueba.
x = [0, 1, 2, 3, 4];
y = [1, 2.2, 2.8, 3.6, 5.1];
grado = 2;

[A, px, err] = minimosCuadrados(x, y, grado);

disp("Coeficientes del polinomio ajustado:");
disp(px);

disp("Error del ajuste:");
disp(err);

graficarPolinomio(px,x,y,[0,5]);
