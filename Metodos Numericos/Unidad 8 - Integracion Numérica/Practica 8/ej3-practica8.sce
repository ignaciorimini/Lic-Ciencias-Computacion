funcprot(0);

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
// Ejercicio a.
function y = f(x)
    y = 1;
endfunction

a = 1;
b = 3;
n = 4;
res = integrate('f','x',a,b);
res1 = metodoSimpsonCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio a.");
disp("Integral real: " + string(res) + "| Integral por metodo de Simpson compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio b.
function y = f(x)
    y = x^3;
endfunction

a = 0;
b = 2;
n = 4;
res = integrate('f','x',a,b);
res1 = metodoTrapecioCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio b.");
disp("Integral real: " + string(res) + "| Integral por metodo de Simpson compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio c.
function y = f(x)
    y = x*(1+x^2)^(1/2);
endfunction

a = 0;
b = 3;
n = 6;
res = integrate('f','x',a,b);
res1 = metodoSimpsonCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio c.");
disp("Integral real: " + string(res) + "| Integral por metodo de Simpson compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio d.
function y = f(x)
    y = sin(%pi*x);
endfunction

a = 0;
b = 1;
n = 8;
res = integrate('f','x',a,b);
res1 = metodoSimpsonCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio d.");
disp("Integral real: " + string(res) + "| Integral por metodo de Simpson compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");


// _____________________________________
// Ejercicio e.
function y = f(x)
    y = x*sin(x);
endfunction

a = 0;
b = 2*%pi;
n = 8;
res = integrate('f','x',a,b);
res1 = metodoSimpsonCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio e.");
disp("Integral real: " + string(res) + "| Integral por metodo de Simpson compuesto: " + string(res1));
disp("Error: " + string(err1))
disp("________________________");

// _____________________________________
// Ejercicio f.
function y = f(x)
    y = x^2*exp(x);
endfunction

a = 0;
b = 1;
n = 8;
res = integrate('f','x',a,b);
res1 = metodoSimpsonCompuesto(f,a,b,n);
err1 = abs(res - res1);
disp("Ejercicio f.");
disp("Integral real: " + string(res) + "| Integral por metodo de Simpson compuesto: " + string(res1));
disp("Error: " + string(err1))
