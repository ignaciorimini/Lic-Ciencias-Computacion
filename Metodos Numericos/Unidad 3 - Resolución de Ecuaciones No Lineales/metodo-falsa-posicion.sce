function [c, iteraciones] = falsaposicion(fun,a,b,err,iter)
    iteraciones = 0;
    fa = fun(a);
    fb = fun(b);
    
    // Chequeamos si el intervalo es válido.
    if (a >= b) then 
        c = %nan;
        error("falsaposicion: Intervalo inválido");
    end
    
    // Chequeamos si los extremos del intervalo tienen distinto signo.
    if (fa*fb > 0) then 
        c = %nan;
        error("falsaposicion: No es posible aplicar el metodo.");
    end

    // Chequeamos si la raíz es un extremo del intervalo.
    if (fa == 0) then
        c = a;
    end;
    
    if (fb == 0) then
        c = b;
    end;
    
    // Inicio del método.
    for i=1:iter
        // Calcular el punto de falsa posición.
        c = b - fb*((b-a)/(fb - fa));
        fc = fun(c);
        
        // Condición de parada.
        if (abs(fc) < err) || (abs(b - a) < err) then
            iteraciones = i;
            return;
        end
        
        // Actualizamos el intervalo.
        if fa*fc < 0 then
            b = c;
            fb = fc;
        else
            a = c;
            fa = fc;
        end
        
        iteraciones = i;
    end
    
    disp("falsaposicion: Se alcanzó la cantidad máxima de iteraciones, el resultado puede ser erróneo");
endfunction

// _____________________________
// Ejemplo. Función x^3 + 2 raíz es -1.2599210498949.
function y = f(x)
    y = x^3 + 2;
endfunction

a = -1.5
b = 0.5;
err = 1e-6;
iter = 100;
[res, iter] = falsaposicion(f,-2,0,err,iter);
disp("Solución: " + string(res));
disp("Iteraciones: " + string(iter));
