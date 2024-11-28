function I = metodoTrapecioBidimensional(fx,a,b,cx,dx,n,m)
    // f: función a integrar, f(x, y)
    // a, b: límites de integración en x
    // c(x), d(x): funciones que definen los límites en y
    // n: número de subintervalos en x
    // m: número de subintervalos en y
    
    hx = (b - a)/n;
    I = 0;
    
    // Iterar sobre los subintervalos de x
    for i=0:n
        xi = a + hx*i;
        
        // Límite inferior y superior en y.
        y1 = cx(xi);
        y2 = dx(xi);
        
        // Paso en y.
        hy = (y2 - y1)/m;
        
        // Regla del trapecio sobre y.
        Iy = (fx(xi,a) + fx(xi,b))/2;
        for j=1:(n-1)
            yj = y1 + j*hy;
            Iy = Iy + fx(xi,yj);
        end
        
        // Agregar contribución del subintervalo.
        I = I + hy*Iy;
    end
    
    I = I * hx;
endfunction

// ____________________________________
// Ejemplo.
function w = f(x,y)
    w = x^2 + y^2;
endfunction

function y = cx(x)
    y = 1;
endfunction

function y = dx(x)
    y = 3;
endfunction

a = 0;
b = 2;
n = 50;
m = 50;
res = metodoTrapecioBidimensional(f,a,b,cx,dx,n,m);
disp("Resultado de la integral: " + string(res));
