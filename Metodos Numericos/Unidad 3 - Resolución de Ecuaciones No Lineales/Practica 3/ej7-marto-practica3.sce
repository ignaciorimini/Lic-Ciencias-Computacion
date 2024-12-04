function result = metodo_newton_multivariable(f, x0, tolerancia, maxIter)
    numIters = 0; // Número de iteraciones del método
    xn = x0; // Inicializamos xn como x0
    
    while numIters < maxIter
        J = numderivative(f, xn); // Calculamos la matriz Jacobiana de f en xn
        
        if det(J) == 0 then
            error("La matriz Jacobiana no es invertible en la iteración " + string(numIters));
        end
        
        J_inv = inv(J); // Invertimos la matriz Jacobiana

        // Calculamos la nueva aproximación
        xn = xn - J_inv * f(xn);
        
        // Incrementamos el contador de iteraciones
        numIters = numIters + 1;

        // Comprobamos la condición de parada
        if norm(f(xn)) < tolerancia then
            break;
        end
    end

    result = xn; // Resultado conseguido
endfunction

// Define el sistema de ecuaciones
function F = sistemaFunciones(x)
    F(1) = 1 + x(1)^2 - x(2)^2 + exp(x(1)) * cos(x(2));
    F(2) = 2 * x(1) * x(2) + exp(x(1)) * sin(x(2));
endfunction

// Valores iniciales y parámetros
x0 = [-1; 4]; // Valor inicial
tolerancia = 1e-6; // Tolerancia
maxIter = 2; // Número máximo de iteraciones

// Llamar al métodO de Newton
solucion = metodo_newton_multivariable(sistemaFunciones, x0, tolerancia, maxIter);
disp(solucion);
