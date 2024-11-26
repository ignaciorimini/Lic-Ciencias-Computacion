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


// Ejercicio. Obtenemos polinomio interpolante y luego calculamos los puntos pedidos.
x = [2.0, 2.1, 2.2, 2.3, 2.4, 2.5];
y = [0.2239, 0.1666, 0.1104, 0.0555, 0.0025, -0.0484];

p = metodoDiferenciasDivididas(x,y);
disp("Polinomio interpolante: ", p);

j1 = horner(p, 2.15);
j2 = horner(p, 2.35);
disp("J(2.15) = " + string(j1));
disp("J(2.35) = " + string(j2));


