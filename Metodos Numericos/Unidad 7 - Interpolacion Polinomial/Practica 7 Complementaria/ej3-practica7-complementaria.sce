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

function A = matrizVandermonde(x,n)
    m = length(x);
    A = ones(m,n);
    
    for j=2:n
        A(:,j) = (x').^(j-1);
    end
endfunction

function [A,px,err] = minimosCuadrados(x,y,grado)    
    n = length(x);
    if n <> length(y) then
        error("minimosCuadrados: x e y deben tener misma cantidad de elementos");
    end
    
    A = matrizVandermonde(x,grado+1)
    
    [coeffs, Aaum] = gausselimPivot(A'*A, A'*y');
    
    px = poly(coeffs, "x", "coeff");
    
    err = norm(A*coeffs - y');
endfunction

// ______________________________________________
// a) Polinomios.
x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
y21 = [145.61, 151.12, 157.27, 164.72, 172.29, 182.91, 185.12, 188.62, 190.09, 197.99, 204.32, 207.97];
y22 = [209.92, 225.82, 265.71, 295.24, 301.62, 311.80, 327.39, 343.10, 366.54, 385.20, 407.77, 428.42];

// Año 2021. Polinomio lineal.
[A, pxLin21, errLin21] = minimosCuadrados(x,y21,1);
disp("Polinomio lineal para 2021: ", pxLin21);

// Año 2021. Polinomio cuadrático.
[A, pxCuad21, errCuad21] = minimosCuadrados(x,y21,2);
disp("Polinomio cuadrático para 2021: ", pxCuad21);

// Año 2021. Polinomio cúbico.
[A, pxCub21, errCub21] = minimosCuadrados(x,y21,3);
disp("Polinomio cúbico para 2021: ", pxCub21);


// Año 2022. Polinomio lineal.
[A, pxLin22, errLin22] = minimosCuadrados(x,y22,1);
disp("Polinomio lineal para 2022: ", pxLin22);

// Año 2022. Polinomio cuadrático.
[A, pxCuad22, errCuad22] = minimosCuadrados(x,y22,2);
disp("Polinomio cuadrático para 2022: ", pxCuad22);

// Año 2022. Polinomio cúbico.
[A, pxCub22, errCub22] = minimosCuadrados(x,y22,3);
disp("Polinomio cúbico para 2022: ", pxCub22);

// ______________________________________________
// b) Gráficos.
puntos = linspace(1,12,100);
pol21EvalLin = horner(pxLin21, puntos);
pol21EvalCuad = horner(pxCuad21, puntos);
pol21EvalCub = horner(pxCub21, puntos);

pol22EvalLin = horner(pxLin22, puntos);
pol22EvalCuad = horner(pxCuad22, puntos);
pol22EvalCub = horner(pxCub22, puntos);

plot(x,y21, '. r');
plot(puntos, pol21EvalLin, 'r');
plot(puntos, pol21EvalCuad, 'g');
plot(puntos, pol21EvalCub, 'b');

plot(x,y22, '. b');
plot(puntos, pol22EvalLin, 'r');
plot(puntos, pol22EvalCuad, 'g');
plot(puntos, pol22EvalCub, 'b');

legend(["datos 2021", "lineal", "cuadratico", "cubico", "datos 2022"])
xgrid();
title("Polinomio lineal");
xlabel("Meses");
ylabel("Precio");


// ______________________________________________
// c) Mejores aproximaciones.
disp("Error lineal 2021: " + string(errLin21));
disp("Error cuadrático 2021: " + string(errCuad21));
disp("Error cúbico 2021: " + string(errCub21));

disp("Error lineal 2022: " + string(errLin22));
disp("Error cuadrático 2022: " + string(errCuad22));
disp("Error cúbico 2022: " + string(errCub22));

// ______________________________________________
// d) Conclusión.
// Viendo los polinomios lineales de cada año, se puede inferir que en 2022 el precio creció más, pues el polinomio lineal tiene mayor pendiente.
