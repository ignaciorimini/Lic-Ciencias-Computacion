function I = metodoTrapecioBidimensional(fx,a,b,cx,dx,n,m)
    hx = (b - a)/n;
    I = 0;
    
    // Iterar sobre los subintervalos de x
    for i=0:n
        xi = a + hx*i;
        
        // Límite inferior y superior en y.
        y1 = cx(xi);
        y2 = dx(xi);
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
// Ejercicio. El area de la circunferencia unitaria deberia dar pi.
function w = f(x,y)
    w = 1;
endfunction

function y = cx(x)
    y = -sqrt(1 - (x-1)^2);
endfunction

function y = dx(x)
    y = sqrt(1 - (x-1)^2);
endfunction

a = 0;
b = 2;
n = 100;
m = 100;
res = metodoTrapecioBidimensional(f,a,b,cx,dx,n,m);
disp("Resultado de la integral: " + string(res));
