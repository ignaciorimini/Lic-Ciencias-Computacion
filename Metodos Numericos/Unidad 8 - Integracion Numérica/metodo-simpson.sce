// Regla de Simpson
function w = metodoSimpson(fx,a,b)
    h = (b-a)/2;
    x1 = a + h;
    w = (fx(a)+4*fx(x1)+fx(b)) * h/3;
endfunction

// Metod0 compuesto de Simpson
function w = metodoSimpsonCompuesto(fx,a,b,n)
    h = (b-a)/n;
    
    // Sumamos los valores extremos
    w = fx(a)+fx(b);
    
    // Sumamos los valores intermedios
    for i=1:2:n-1
        xi = a + i*h;
        w = w + 4*fx(xi);
    end
    
    for i=2:2:n-1
        xi = a + i*h;
        w = w + 2*fx(xi);
    end
    
    w = h/3 * w;
endfunction

// Ejemplo de prueba: funcion x^2 de integral x^3/3 -> integral = 0.3333 en [0,1]
function y = f(x)
    y = x.^2;
endfunction

a = 0;
b = 1;
n = 8;
res = metodoSimpson(f,a,b);
disp("Integracion por metodo de Simpson: ", res);

res2 = metodoSimpsonCompuesto(f,a,b,n);
disp("Integracion por metodo de Simpson Compuesto: ", res2);
