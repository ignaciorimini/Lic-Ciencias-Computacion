function y = LK(x,k)
    n = length(x);
    r = [x(1:k-1) x(k+1:n)];
    p = poly(r, "x", "roots");
    pk = horner(p, x(k));
    y = p / pk;
endfunction

function z = interpolacionLagrange(x,y)
    n = length(x);
    pol = 0;
    
    for k=1:n
        pol = pol + (LK(x,k)*y(k));
    end
    
    z = pol;
endfunction

function w = difDiv(x,y)
    n = length(x);
    
    if n == 1 then
        w = y(1);
    else
        w = (difDiv(x(2:n), y(2:n)) - difDiv(x(1:n-1), y(1:n-1)))/(x(n) - x(1));
    end
endfunction

function p = metodoDiferenciasDivididas(x,y)
    n = length(x);
    p = 0;
    r = poly(0, "x");
    
    for i=n:(-1):2
        D = difDiv(x(1:i),y(1:i));
        p = (p + D)*(r - x(i-1));
    end
    
    p = p + y(1);
endfunction

// a) Ejemplo del ejercicio.
x = [0, 0.2, 0.4, 0.6];
y = [1.0, 1.2214, 1.4918, 1.8221];

p1 = interpolacionLagrange(x,y);
r1 = horner(p1,1/3);
disp("Valor de e^(1/3) Lagrange: " + string(r1));

p2 = metodoDiferenciasDivididas(x,y);
r2 = horner(p2,1/3);
disp("Valor de e^(1/3) DD: " + string(r2));

// b) Cotas de los errores (hoja) y error exacto.
cota = ((1/3 - 0)*(1/3 - 0.2)*(1/3 - 0.4)*(1/3 - 0.6))/24;
cota = abs(cota * %e^0.6);
disp("Cota del error: " + string(cota));

err1 = abs(r1 - 1.395612425);
disp("Error por Lagrange: " + string(err1));

err2 = abs(r2 - 1.395612425);
disp("Error por DD: " + string(err2));
