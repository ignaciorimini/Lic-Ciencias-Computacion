function y = LK(x,k)
    n = length(x);
    r = [x(1:k-1) x(k+1:n)];
    p = poly(r, "x", "roots");
    pk = horner(p, x(k));
    y = p / pk;
endfunction

function px = interpolacionLagrange(x,y)
    n = length(x);
    pol = 0;
    
    for k=1:n
        pol = pol + (LK(x,k)*y(k));
    end
    
    px = pol;
endfunction

// ___________________________________
// Función del ejercicio: e^x.
function y = f(x)
    y = exp(x);
endfunction

// Vector de nodos equiespaciados y cálculo del vector y.
x = [-1, -1/3, 1/3, 1];
y = f(x);

// a) Polinomio de interpolación.
px = interpolacionLagrange(x,y);
disp("Polinomio Lagrange: ", px);

// b) Cálculo de valores dados.
aprox1 = horner(px, -0.9);
aprox2 = horner(px, 0.01);
real1 = exp(-0.9);
real2 = exp(0.01);
err1 = abs(real1 - aprox1);
err2 = abs(real2 - aprox2);
disp("p(-0.9) = " + string(aprox1) + " | exp(-0.9) = " + string(real1) + " | Error = " + string(err1));
disp("p(0.01) = " + string(aprox2) + " | exp(0.01) = " + string(real2) + " | Error = " + string(err2));

// ___________________________________
// c) Cotas del error.
// f(x) - p(x) = ((x - x0)(x - x1)(x - x2)(x - x3))/24 * f''''(c)

// f(-0.9) - p(-0.9) = phi(x)/24 * e^c, para algún c en [-1,1]
// Como e^x es creciente, el maximo lo asume en el extremo del intervalo: c = 1.
phi = (-0.9 - (-1)) * (-0.9 -(-1/3)) * (-0.9 - (1/3)) * (-0.9 - 1);
cotaErr1 = abs(phi/24 * exp(1));
disp("Cota del error p(-0.9) <= " + string(cotaErr1));

// f(0.01) - p(0.01) = phi(x)/24 * e^c, para c = 1.
phi = (0.01 - (-1)) * (0.01 -(-1/3)) * (0.01 - (1/3)) * (0.01 - 1);
cotaErr2 = abs(phi/24 * exp(1));
disp("Cota del error p(0.01) <= " + string(cotaErr2));

// ___________________________________
// d) Cota del error p(x) en [-1,1].
// f(x) - p(x) = 1/24 * max phi * max f''''(c)
// max f''''(c) en [-1,1] es c=1 pues f''''(x) = exp(x) que es creciente.
// phi = (x - x0)*(x - x1)*(x - x2)*(x - x3) = (x - (-1))*(x - (-1/3))*(x - (1/3))*(x - 1)
x = poly(0, "x");
phi = (x - (-1)) * (x -(-1/3)) * (x- (1/3)) * (x - 1);

// Evaluamos phi en puntos de [-1,1] y calculamos el máximo.
puntos = linspace(-1,1,100);
maxPhi = max(abs(horner(phi, puntos)));
cotaErr3 = (maxPhi/24) * exp(1);
disp("Cota del error p(x) <= " + string(cotaErr3));

// ___________________________________
// e) Polinomio con Chebyshev.
function x = raicesPolChebyshev(n)
    if n <= 0 then
        error("raicesPolChebyshev: n debe ser un entero positivo");
    end
    
    x = zeros(1, n);
    
    for k=1:n
        x(k) = cos(((2*k - 1)*%pi)/(2*n));
    end
endfunction

function Pn = interpolacionChebyshevLagrange(f,n)
    x = raicesPolChebyshev(n);
    
    y = zeros(1,n);
    y = f(x);
    
    Pn = interpolacionLagrange(x,y);
endfunction

n = 4;
qx = interpolacionChebyshevLagrange(f,n)
disp("Polinomio Chebyshev:", qx);


// ___________________________________
// f) Cota del error de q(x) obtenido por Chebyshev.
// f(x) - q(x) = 1/4! * max phi(x) * max f''''(c), para algun c en [-1,1]
raices = raicesPolChebyshev(n);
phi = (x - raices(1)) * (x - raices(2)) * (x - raices(3)) * (x - raices(4));
puntos = linspace(-1,1,100);
maxPhi = max(abs(horner(phi, puntos)));
cotaErr4 = maxPhi/24 * exp(1);
disp("Cota del error q(x) <= " + string(cotaErr4));

// ___________________________________
// g) Gráfico de los errores de p(x) y q(x).
puntos = linspace(-1,1,100);
errP = f(puntos) - horner(px, puntos);
errQ = f(puntos) - horner(qx, puntos);

plot(puntos, errP, 'r');
plot(puntos, errQ, 'r');
xgrid();
title("Error de interpolación");
xlabel("x");
ylabel("Error");

