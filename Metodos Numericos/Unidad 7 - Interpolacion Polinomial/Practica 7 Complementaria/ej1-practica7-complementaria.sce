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

function graficarPolinomio(px,x,y)
    // Puntos originales
    plot(x, y, '.'); // Puntos originales marcados con círculos
    
    // Crear una densidad más alta de puntos para graficar el polinomio ajustado
    x_denso = linspace(min(x), max(x), 100); // 100 puntos equidistantes en el rango de x
    y_ajustado = horner(px, x_denso); // Evaluar el polinomio px en los puntos x_denso
    
    // Graficar el polinomio ajustado
    plot(x_denso, y_ajustado, '-r'); // Curva ajustada en rojo
    legend(["Datos originales", "Polinomio ajustado"]);
    
    // Títulos y etiquetas
    title("Ajuste por Mínimos Cuadrados");
    xgrid();
    xlabel("x");
    ylabel("y");
endfunction

// _____________________________________
// Ejercicio.
function A = crearMatriz(x)
    n = length(x);
    A(:,1) = x.^2;
    A(:,2) = x.^3;
    A(:,3) = x.^4;
endfunction

function [px,err] = minimosCuadradosMod(x,y,A)
    [coeffs, Aaum] = gausselimPivot(A'*A, A'*y');
    x = poly(0, "x");
    px = coeffs(1)*x^2 + coeffs(2)*x^3 + coeffs(3)*x^4;
    err = norm(A*coeffs - y');
endfunction

x = [-2.0, -1.6, -1.2, -0.8, -0.4, 0, 0.4, 0.8, 1.2, 1.6, 2.0];
y = [1.50, 0.99, 0.61, 0.27, 0.02, -0.0096, 0.065, 0.38, 0.63, 0.98, 1.50];
grado = 4;

A = crearMatriz(x);

[px, err] = minimosCuadradosMod(x,y,A);

disp("Coeficientes del polinomio ajustado:");
disp(px);

disp("Error del ajuste:");
disp(err);

graficarPolinomio(px,x,y);
