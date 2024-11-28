// ___________________________
// Ejercicio.
function A = crearMatrizVandermonde(x,grado)
    n = length(x);
    A = ones(n,grado+1);
    
    for j=2:grado+1
        A(:,j) = (x').^(j-1);
    end
endfunction

x = [1,2,3,4,5,6,7,8,9,10];
yMax = [32.9, 30.8, 26.4, 24.2, 19.2, 16.5, 19.3, 21, 23, 26.2];

// __________________________
// a) Obtener p3, p5, p7 y p9.
A3 = crearMatrizVandermonde(x,3);
A5 = crearMatrizVandermonde(x,5);
A7 = crearMatrizVandermonde(x,7);
A9 = crearMatrizVandermonde(x,9);

coeff3 = inv(A3'*A3)*(A3'*yMax');
p3 = poly(coeff3, "x", "coeff");

coeff5 = inv(A5'*A5)*(A5'*yMax');
p5 = poly(coeff5, "x", "coeff");

coeff7 = inv(A7'*A7)*(A7'*yMax');
p7 = poly(coeff7, "x", "coeff");

coeff9 = inv(A9'*A9)*(A9'*yMax');
p9 = poly(coeff9, "x", "coeff");

// Gráficar polinomios.
puntos = 1:0.1:10;
p3Eval = horner(p3,puntos);
p5Eval = horner(p5,puntos);
p7Eval = horner(p7,puntos);
p9Eval = horner(p9,puntos);

plot(x,yMax,".");
plot(puntos,p3Eval,"r");
plot(puntos,p5Eval,"b");
plot(puntos,p7Eval,"m");
plot(puntos,p9Eval,"k");


xgrid();
title("Promedio mensual de temperatura máxima.")
xlabel("Meses");
ylabel("Temperatura (°C)");
legend(["datos","p3","p5","p7","p9"]);

// __________________________
// b) Factorizacion QR.
function [Q, R] = matricesQR(A)
    [m, n] = size(A);
    
    if rank(A) <> n then
        error("matricesQR: Las columnas de A no son lineal independientes");
    end
    
    Q = zeros(m, n);
    R = zeros(n, n);
    
    for k=1:n
        v = A(:,k);
        
        for j=1:k-1
            R(j, k) = Q(:, j)' * v;
            v = v - R(j, k)*Q(:,j);
        end
        
        R(k, k) = norm(v);
        Q(:, k) = v / R(k,k)
    end
endfunction

// Sistema QR en minimos cuadrados -> R*x = Q'*b
// Aplicamos QR a las matrices A3, A5, A7 y A9.
//[Q3, R3] = matricesQR(A3);
//coeff3 = inv(R3)*(Q3'*yMax);
//
//[Q5, R5] = matricesQR(A5);
//coeff5 = inv(R5)*(Q5'*yMax);
//
//[Q7, R7] = matricesQR(A7);
//coeff7 = inv(R7)*(Q7'*yMax);
//
//[Q9, R9] = matricesQR(A9);
//coeff9 = inv(R9)*(Q9'*yMax);

// __________________________
// c) La matriz de Vandermonde en cada caso está mal condicionada y trae problemas.

// __________________________
// d) Elección de mejor polinomio.
// El polinomio p9(x) usaría porque es el que menos error tiene.

err3 = norm(A3*coeff3 - yMax');
err5 = norm(A5*coeff5 - yMax');
err7 = norm(A7*coeff7 - yMax');
err9 = norm(A9*coeff9 - yMax');
disp(err3);
disp(err5);
disp(err7);
disp(err9);


