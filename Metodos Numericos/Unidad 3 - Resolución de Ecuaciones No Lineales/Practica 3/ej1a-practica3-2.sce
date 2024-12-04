function y = falsaposicion(f,a,b,err,iter)
    for i=1:iter
        fa = f(a);
        fb = f(b);
        
        // Calcular el punto de falsa posición.
        c = b - fb*((b-a)/(fb - fa));
        fc = f(c);
        
        // Condición de parada.
        if abs(fc) < err then
            y = c;
            disp("Iteraciones: " + string(i));
            return;
        end
        
        // Actualizamos el intervalo.
        if fa*fc < 0 then
            b = c;
        else
            a = c;
        end
    end
    
    y = c;
endfunction

// Código para probar. Función x^3 + 2 raíz es -1.2599210498949.
// deff("y=f(x)", "y=x^3+2");
// format(16);
// res = falsaposicion(f,-1.5,0.5,0.000001,100)

function  y = f(x)
    y = x^5 - 3*x^4 + 10*x - 8;
endfunction

res = falsaposicion(f, 2, 5, 0.000001, 150);
disp(res);
