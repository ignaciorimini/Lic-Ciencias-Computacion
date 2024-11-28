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

// Recibe vector x de nodos y vector y de puntos tal que p(xi) = yi.
// Devuelve el polinomio interpolante resolviendo Xa=y.
function p = metodoVandermonde(x,y)
    // Verificar que los valores de x sean todos distintos.
    n = length(x);
    valoresUnicos = unique(x);
    if n <> length(valoresUnicos) then
        error("metodoVandermonde: los nodos del vector x deben ser distintos");
    end
    
    // Verificar dimensiones de x e y.
    if n <> length(y) then
        error("metodoVandermonde: no coinciden dimensiones de x e y");
    end
    
    // Construir matriz X de Vandermonde.
    X = ones(n,n);
    
    for j=2:n
        X(:,j) = x .^(j-1);
    end
    
    disp("Matriz Vandermonde:", X);
    
    // Resolver sistema Xa=y y construir polinomio.
    [coeffs,Aaum] = gausselim(X,y');
    p = poly(coeffs, "x", "coeff");
endfunction

// Ejemplo. Polinomio que pase por (1,-1),(2,-1),(3,7),(4,8).
// Resultado: p(x) = 22 -39.5x +19x² -2.5x³ (ver en Geogebra).
x = [1, 2, 3, 4];
y = [-1, -1, 7, 8];

p = metodoVandermonde(x,y);
disp("Polinomio interpolante: ", p);
