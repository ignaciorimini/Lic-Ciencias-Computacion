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

// Funcion que recibe un vector x = [x0, x1, ... xn] de nodos, el grado n (columnas)
// y construye la matriz de Vandermonde.
function A = matrizVandermonde(x,n)
    m = length(x);
    A = ones(m,n);
    
    for j=2:n
        A(:,j) = (x').^(j-1);
    end
endfunction

// ________________________________
// Ejercicio.
x = 0:1:30;
y = [35,23,47,59,82,113,143,179,233,269,303,335,371,404,434,446,457,470,481,482,476,465,454,436,424,397,385,359,340,322,303];

function y = construirVectorTotal(v)
    n = length(v);
    y = zeros(1,n)
    sumatoria = 0;
    
    for i=1:n
        sumatoria = sumatoria + v(i);
        y(i) = sumatoria;
    end
endfunction

yTotal = construirVectorTotal(y);

// g(t) = θ_1 * e^(−θ_2 * e^(−θ_3 * t))
// ln(g(t)) = ln(θ_1) − θ_2 * e^(−θ_3 * t)
// ln(g(t)) - ln(θ_1) = − θ_2 * e^(−θ_3 * t)
// -ln(g(t)/θ_1) = θ_2 * e^(−θ_3 * t)
// ln(-ln(g(t)/θ_1)) = ln(θ_2 * e^(−θ_3 * t))
// ln(-ln(g(t)/θ_1)) = ln(θ_2) − θ_3 * t
// ln(ln(θ_1/g(t)) = ln(θ_2) − θ_3 * t

// ln(ln(θ_1/y)) = ln(θ_2) − θ_3 * t

// Ajustamos θ_2 y θ_3 por mínimos cuadrados en la función h.
n = length(x);
A = ones(n,2);
A(:,2) = x;

// Resolver sistema At*A*x = At*y
b = log(log(13129.3./yTotal))
[coeffs, Aaum] = gausselimPivot(A'*A, A'*b');

lnAngulo2 = coeffs(1);
angulo2 = exp(coeffs(1));
angulo3 = -coeffs(2);
disp(angulo2);
disp(angulo3);

function y = g(t)
    y = 13129.3 * exp(-angulo2 * exp(-angulo3 .* t));
endfunction

puntos = linspace(0,31,1000);
funEval = g(puntos);

plot(x,yTotal,'.');
plot(puntos,funEval,"r");
xgrid();
