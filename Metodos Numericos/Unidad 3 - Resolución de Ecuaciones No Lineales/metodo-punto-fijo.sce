function [y, iteraciones] = puntofijo(g,x0,err,iter)
    iteraciones = 0;
    xn = x0;
    
    for i=1:iter;
        xn1 = g(xn);
        
        if abs(xn1 - xn) < err then
            y = xn1;
            iteraciones = i;
            return;
        end
        
        xn = xn1;
        iteraciones = i;
    end
    
    y = xn;
endfunction

// ______________________
// Ejemplo.
function y = f(x)
    y = cos(x);
endfunction

x0 = 1;
err = 1e-6;
iter = 100;
[res, iter] = puntofijo(f,x0,err,iter);
disp("Solución: " + string(res));
disp("Iteraciones: " + string(iter));
