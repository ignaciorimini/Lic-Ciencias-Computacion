function [c, iteraciones] = metodoBiseccion(fun,a,b,err,maxIter)
    iteraciones = 0;
    fA = fun(a);
    fB = fun(b);
    
    // Chequeamos si el intervalo es válido.
    if (a >= b) then 
        c = %nan;
        error("metodoBiseccion: Intervalo inválido");
    end
    
    // Chequeamos si los extremos del intervalo tienen distinto signo.
    if (fA*fB > 0) then 
        c = %nan;
        error("metodoBiseccion: No es posible aplicar bisección.");
    end

    // Chequeamos si la raíz es un extremo del intervalo.
    if (fA == 0) then
        c = a;
    end;
    
    if (fB == 0) then
        c = b;
    end;

    // Inicio del método.
    c = (a+b)/2;
        
    while (b - c > err) && (iteraciones < maxIter)
        iteraciones = iteraciones + 1;
        fC = fun(c);
        
        if (fC == 0) then
            return;
        elseif (fun(b)*fC < 0) then
            a = c;
        else 
            b = c;
        end
        
        c = (a+b)/2;    
    end
    
    // Chequeamos si se terminó por cantidad de iteraciones o por condición de parada.
    if (b - c > err) then
        error("metodoBiseccion: Se alcanzó la cantidad máxima de iteraciones.");
    end
endfunction

// _________________________________
// Ejemplo. Función x^3 + 2 raíz es -1.2599210498949.
function y = f(x)
    y = x^3 + 2;
endfunction

a = -1.5;
b = 0.5;
err = 1e-6;
iteraciones = 100;
[res iter] = metodoBiseccion(f,-100,100,err,100);
disp("Solución: " + string(res));
disp("Iteraciones: " + string(iter));
