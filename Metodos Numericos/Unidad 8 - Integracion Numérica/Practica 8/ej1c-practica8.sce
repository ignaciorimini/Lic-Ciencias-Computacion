// Metod0 del trapecio simple (sin el error)
function y = metodoTrapecio(f,a,b)
    y = (f(a)+f(b)) * (b-a)/2;
endfunction

// Metod0 del trapecio compuesto
function y = metodoTrapecioCompuesto(f,a,b,n)
    h = (b-a)/n;
    
    y = (fx(a)+fx(b))/2;
    
    for i=1:(n-1)
        xi = a + h*i;
        y = y + fx(xi);
    end
    
    y = h*y;
endfunction

// Regla de Simpson
function w = metodoSimpson(f,a,b)
    h = (b-a)/2;
    x1 = a + h;
    w = (f(a)+4*f(x1)+f(b)) * h/3;
endfunction

// Metod0 compuesto de Simpson
function w = metodoSimpsonCompuesto(f,a,b,n)
    h = (b-a)/n;
    
    w = f(a)+f(b);
    
    for i=1:2:n-1
        xi = a + i*h;
        w = w + 4*f(xi);
    end
    
    for i=2:2:n-1
        xi = a + i*h;
        w = w + 2*f(xi);
    end
    
    w = h/3 * w;
endfunction

// _____________________________________
// Ejercicio. Función ln(x) -> integral = xln(x) - x -> 2ln(2) - 2 + 1 = 2ln(2) - 1 = 0.386294
function y = fx(x)
    y = sin(x)^2;
endfunction

// Valor de la integral real.
a = 0;
b = %pi/3;
n = 8;

res = integrate('fx','x',a,b);
disp("Integral real: ", res);

// Aproximación con métodos vistos.
resTrapecio = metodoTrapecio(fx,a,b);
errTrapecio = abs(res - resTrapecio);
disp("Integral por metodo del trapecio simple: " + string(resTrapecio));
disp("Error trapecio simple: " + string(errTrapecio));

resTrapecioComp = metodoTrapecioCompuesto(fx,a,b,n);
errTrapecioComp = abs(res - resTrapecioComp);
disp("Integral por metodo del trapecio compuesto: " + string(resTrapecioComp));
disp("Error trapecio compuesto: " + string(errTrapecioComp));

resSimpson = metodoSimpson(fx,a,b);
errSimpson = abs(res - resSimpson);
disp("Integracion por metodo de Simpson simple: " + string(resSimpson));
disp("Error Simpson simple: " + string(errSimpson));

resSimpsonComp = metodoSimpsonCompuesto(fx,a,b,n);
errSimpsonComp = abs(res - resSimpsonComp);
disp("Integracion por metodo de Simpson compuesto: " + string(resSimpsonComp));
disp("Error Simpson compuesto: " + string(errSimpsonComp));
