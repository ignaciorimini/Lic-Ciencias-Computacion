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

// Ejercicio.
x = [0, 1, 2, 3];
y = [1, 3, 3, 3];

p = interpolacionLagrange(x, y);
disp("Polinomio interpolante:", p);

res = horner(p,2.5);
disp("P(2.5) = " + string(res));
