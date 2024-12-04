// Función que toma un vector de funciones (sistema de ecuaciones), un vector de variables a evaluar el sistema de ecuaciones, un error para condicion de parada y un numero maximo de iteraciones.
function y = newtonMultivariable(sisFunciones, vectorVariables, err, iter)
    for i=1:iter;
        // Calcular la matriz jacobiana.
        J = numderivative(sisFunciones, vectorVariables);
        
        // Verificar si la matriz jacobiana es invertible.
        if det(J) == 0 then
            error("La matriz jacobiana no es invertible.");
        end
        
        // Calcular matriz Jacobiana inversa.
        inversaJ = inv(J);
        
        // Calcular el vector de funciones F en las variables actuales.
        F = sisFunciones(vectorVariables);
       
        // Calcular el nuevo vector de variables.
        delta = -inversaJ * F;
        vectorVariables = vectorVariables + delta;
       
        // Condición de parada.
        if norm(delta) < err then
            break;
        end
    end
    
    y = vectorVariables;
endfunction

// Funcion que toma un vector de variables y devuelve un vector de funciones aplicadas en esas variables.
// Es decir, retorna el sistema de ecuaciones no lineales del ejercicio 7.
// vectorVariables = (x1.0, x2.0, x3.0, ...)
function Fx = sistemaFunciones(vectorVariables)
    x = vectorVariables;
    Fx(1) = 1 + x(1)^2 - x(2)^2 + exp(x(1))*cos(x(2));
    Fx(2) = 2*x(1)*x(2) + exp(x(1))*sin(x(2));
endfunction

// Probar funcion:
vectorInicial = [-1; 4];
errorDeseado = 1e-6;
maxIteraciones = 1000;
solucion = newtonMultivariable(sistemaFunciones, vectorInicial, errorDeseado, maxIteraciones);
disp(solucion);

