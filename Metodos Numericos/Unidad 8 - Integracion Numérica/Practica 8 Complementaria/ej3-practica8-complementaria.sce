// Metod0 del trapecio compuesto
function y = metodoTrapecioCompuesto(fx,a,b,n)
    // Longitud de los intervalos.
    h = (b-a)/n;
    
    // Sumamos los términos extremos (f(x0) y f(xn))
    y = (fx(a)+fx(b))/2;
    
    // Sumamos los términos intermedios f(xi).
    for i=1:(n-1)
        xi = a + h*i;
        y = y + fx(xi);
    end
    
    // Multiplicamos tod0 por h.
    y = h*y;
endfunction

// _________________________________
// Ejercicio.
function y = funcionDentro(x)
    y = sin(x)/x;
end


function y = funcionEj(b,n)
    if b <= 1 then
        error("funcionEj: b debe ser mayor que 1");
    end
    
    y = metodoTrapecioCompuesto(funcionDentro,1,b,n);
endfunction

res = funcionEj(2,100);
disp(res);

res = funcionEj(4,100);
disp(res);

res = funcionEj(6,100);
disp(res);

res = funcionEj(8,100);
disp(res);
