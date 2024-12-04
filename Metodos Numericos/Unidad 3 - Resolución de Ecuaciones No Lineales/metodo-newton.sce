function [y, iteraciones] = metodoNewton(fun,dfun,x0,err,iter)
    for i=1:iter
        fx0 = fun(x0);
        dfx0 = dfun(x0);
        
        // Condición de parada
        if abs(fx0) < err then
            y = x0;
            iteraciones = i;
            return;
        end
        
        // Evitar divisiones por valores pequeños en la derivada
        if abs(dfx0) < 1e-10 then
            disp("metodoNewton: Derivada cercana a cero. Método falló.");
            y = x0;
            iteraciones = i;
            return;
        end
        
        // Actualización de x0 usando la fórmula de Newton
        x1 = x0 - (fx0/dfx0);
        
        // Segunda condicion de parada
        if abs(x1 - x0) < err then
            y = x1;
            iteraciones = i;
            return;
        end
        
        x0 = x1;
        iteraciones = i;
    end
    
    y = x0;
    disp("metodoNewton: Se alcanzó la cantidad máxima de iteraciones, el resultado puede ser erróneo");
endfunction

// ________________________________________
// Ejemplo de prueba. Función x^3 + 2 raíz es -1.2599210498949. 
function y = f(x)
    y = x^3 + 2;
endfunction

function y = df(x)
    y = 3*x^2;
endfunction

x0 = -1.5
err = 1e-6;
iteraciones = 100;
[res, iter] = metodoNewton(f,df,x0,err,iteraciones);

disp("Solución: " + string(res));
disp("Iteraciones: " + string(iter));
