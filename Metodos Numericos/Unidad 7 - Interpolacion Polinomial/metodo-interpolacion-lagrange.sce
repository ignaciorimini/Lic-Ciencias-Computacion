// Función que calcula los Lk(x).
// Recibe un vector x = (x1, x2, ... xk) y el numero k y retorna el polinomio Lk(x)
function y = LK(x,k)
    n = length(x);              // Tamaño del vector x.
    r = [x(1:k-1) x(k+1:n)];    // Vector igual a x pero sin xk.
    p = poly(r, "x", "roots");  // Vector a partir de r pero raices (x-x1)(x-x2)... (numerador)
    pk = horner(p, x(k));       // Evalua p(xk) que es el denominador.
    y = p / pk;                 // Construimos Lk(x).
endfunction

// Función que calcula el polinomio de interpolación de Lagrange p(x).
// Recibe un vector x = (x1, x2, ... xn) y un vector y = (y1, y2, ... yn),
// estos valores son los puntos (x1, y1), (x2, y2), ... (xn, yn)
function z = interpolacionLagrange(x,y)
    n = length(x);
    pol = 0;
    
    for k=1:n
        pol = pol + (LK(x,k)*y(k));
    end
    
    z = pol;
endfunction

// Ejemplo. Polinomio que pase por (1,-1),(2,-1),(3,7),(4,8).
// Resultado: p(x) = 22 -39.5x +19x² -2.5x³ (ver en Geogebra).
x = [1, 2, 3, 4];
y = [-1, -1, 7, 8];

p = interpolacionLagrange(x, y);
disp("Polinomio interpolante:", p);
