function [y, iteraciones] = secante(fun,x0,x1,err,iter)
    iteraciones = 0;
    
    for i=1:iter
        fx0 = fun(x0);
        fx1 = fun(x1);
        
        // Verificar para evitar división por cero.
        if fx1 - fx0 == 0 then
            disp("Error: f(x1) - f(x0) es demasiado pequeño");
            y = x1;
            iteraciones = i;
            return;
        end
        
        // Aplicar la fórmula de la secante
        x2 = x1 - fx1*((x1 - x0)/(fx1 - fx0));
        
        // Condición de parada
        if (abs(fun(x2)) < err) || (abs(x2 - x1) < err) then
            y = x2;
            iteraciones = i;
            return;
        end
        
        // Actualizar los valores de x0 y x1 para la siguiente iteración
        x0 = x1;
        x1 = x2;
        iteraciones = i;
    end
    
    y = x2;
    disp("secante: Se alcanzó la cantidad máxima de iteraciones, el resultado puede ser erróneo");
endfunction

// _____________________
// Ejemplo. Función x^3 + 2 raíz es -1.2599210498949.
function y = f(x)
    y = x^3 + 2;
endfunction

x0 = -1.5;
x1 = 0.5;
err = 1e-6;
iter = 100;

[res, iter] = secante(f,x0,x1,err,iter);
disp("Solución: " + string(res));
disp("Iteraciones: " + string(iter));
