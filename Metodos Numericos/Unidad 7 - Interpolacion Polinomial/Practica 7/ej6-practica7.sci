// a) Construcción del polinomio interpolante por diferencias divididas de newton.
r = poly(0, "x");
x = [-1, 1, 2, 4];
p = 2 + (r - x(1))*1 + (r - x(1))*(r - x(2))*(-2) + (r - x(1))*(r - x(2))*(r - x(3))*2;
disp("Polinomio interpolante: ", p);

// b) Evaluar p(0).
res = horner(p, 0);
disp("P(0) = " + string(res));
