function y = f(x)
    y = exp(x) - 3*x;
endfunction

function y = secante(f,x0,x1,err,iter)
    for i=1:iter
        fx0 = f(x0);
        fx1 = f(x1);
        if fx1 - fx0 == 0 then
            y = x1;
            return;
        end
        
        // Aplicar la fórmula de la secante
        x2 = x1 - fx1*((x1 - x0)/(fx1 - fx0));
        
        // Condición de parada
        if abs(f(x2)) < err then
            y = x2;
            disp("Iteraciones: " + string(i));
            return;
        end
        
        // Actualizar los valores de x0 y x1 para la siguiente iteración
        x0 = x1;
        x1 = x2;
    end
    
    // Si se llega al número máximo de iteraciones sin encontrar la raíz
    y = x2;
endfunction

res = secante(f, 0, 1, 0.0001, 100);
