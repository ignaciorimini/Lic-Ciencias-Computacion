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

// ____________________________________
// Ejercicio.
// a,b,c son constantes a ajustar.
// Phi1(x,v) = 1, Phi2(x,v) = x, Phi3(x,v) = v.

x = [0,0,1,2,2,2];
v = [0,1,0,0,1,2];
y = [1.42,1.85,0.78,0.18,0.60,1.05];

function A = crearMatrizVandermonde(x,v)
    n = length(x);
    A = ones(n,3);
    A(:,2) = x;
    A(:,3) = v;
endfunction

// Resolver sistema de minimos cuadrados At*A*x = At*y
A = crearMatrizVandermonde(x,v);
[coeffs, Aaum] = gausselimPivot(A'*A, A'*y');
    
// Construir polinomio con los coeficientes obtenidos.
// Mostrar los coeficientes
disp("Coeficientes a, b, c:");
disp(coeffs);

// Crear una malla para el gráfico del plano
[xg, vg] = meshgrid(0:0.1:2, 0:0.1:2);  // Crear malla de puntos para x y v
yg = coeffs(1) + coeffs(2)*xg + coeffs(3)*vg;  // Calculamos el valor de y para la malla

// Graficar los datos en 3D
scatter3d(x, v, y, 1, "r");  // Datos originales en rojo
set(gca(), "auto clear", "off");  // Evitar que se borre el gráfico
plot3d(xg, vg, yg);  // Graficar el plano ajustado
