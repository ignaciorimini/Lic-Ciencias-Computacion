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

function y = cosh(x)
    y = (exp(x) + exp(-x)) / 2;
endfunction

function  y = f1(x)
    y = cos(x) .*cosh(x) + 1;
endfunction

function y = f2(x)
    y = 2*sin(x)-x^2;
endfunction

function y = f3(x)
    y = exp(-x)-x^4;
endfunction

function y = f4(x)
    y = log(x)-x+1;
endfunction

function y = f5(x)
    y = ((x^2)/4) - sin(x);
endfunction

res1 = falsaposicion(f1, -2, 0, 1e-6, 100); // -1.8751039 - 7 iters
disp("Raíz de f1: " + string(res1));

res2 = falsaposicion(f2, -2, 2, 1e-6, 100); // 1.4044152 - 14 iters
disp("Raíz de f2: " + string(res2));

res2 = falsaposicion(f2, -2, 0.5, 1e-6, 100); // 0.0000003 - 13 iters
disp("Raíz de f2: " + string(res2));

res3 = falsaposicion(f3, -2, 0, 1e-6, 100); // -1.4296118 - 28 iters
disp("Raíz de f3: " + string(res3));

res3 = falsaposicion(f3, 0, 1, 1e-6, 100); // 0.8155533 - 11 iters
disp("Raíz de f3: " + string(res3));

res4 = falsaposicion(f4, 0.5, 1.5, 1e-6, 100); // 1.0038118 - 100 iters
disp("Raíz de f4: " + string(res4));

res5 = falsaposicion(f5, -1, 0.5, 1e-6, 100); // 0.0000004 - 6 iters
disp("Raíz de f5: " + string(res5));

res5 = falsaposicion(f5, 0.5, 3, 1e-6, 100); // 1.9337532 - 15 iters
disp("Raíz de f5: " + string(res5));
