function [y, iteraciones] = newtonMultivariable(sisFunciones, vectorVariables, err, iter)
    iteraciones = 0;
    
    for i=1:iter;
        J = numderivative(sisFunciones, vectorVariables);
        
        // Verificar si la matriz jacobiana es aproximadamente singular (no invertible).
        if abs(det(J)) < 1e-10 then
            error("La matriz jacobiana no es invertible.");
            y = vectorVariables;
            iteraciones = i;
            return;
        end
        
        // Calcular el nuevo vector de variables (delta).
        inversaJ = inv(J);
        F = sisFunciones(vectorVariables);
        delta = -inversaJ * F;
        vectorVariables = vectorVariables + delta;
       
        // Condición de parada.
        if norm(delta) < err then
            y = vectorVariables;
            iteraciones = i;
            return;
        end
        
        iteraciones = i;
    end
    
    y = vectorVariables;
endfunction

// Probar funcion:
function Fx = sistemaFunciones(vectorVariables)
    x = vectorVariables;
    Fx(1) = 1 + x(1)^2 - x(2)^2 + exp(x(1))*cos(x(2));
    Fx(2) = 2*x(1)*x(2) + exp(x(1))*sin(x(2));
endfunction

vectorInicial = [-1; 4];
errorDeseado = 1e-6;
maxIteraciones = 1000;
[solucion, iteraciones] = newtonMultivariable(sistemaFunciones, vectorInicial, errorDeseado, maxIteraciones);
disp("Solución: " + string(solucion));
disp("Iteraciones: " + string(iteraciones));

