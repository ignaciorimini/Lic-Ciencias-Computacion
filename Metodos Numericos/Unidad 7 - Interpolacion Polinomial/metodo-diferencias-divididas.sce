// Funcion que recibe un vector x de nodos x0, x1, ... xn
// Y un vector y que representa y0 = f(x0), y1 = f(x1)...
// Y calcula la diferencia dividida dependiendo el tamaño de x 
// (si x tiene 3 puntos calcula diferencia dividida de orden 3).
function w = difDiv(x,y)
    n = length(x);
    
    if n == 1 then
        w = y(1);
    else
        w = (difDiv(x(2:n), y(2:n)) - difDiv(x(1:n-1), y(1:n-1)))/(x(n) - x(1));
    end
endfunction

// Función que recibe vector x de nodos x0,x1,...xn
// Y un vector y de puntos y0 = f(x0), ... yn = f(xn)
// Y realiza el metod0 de las diferencias divididas de Newton.
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

// Función para graficar el polinomio interpolante.
function graficarPolinomio(p, xNodos, yNodos, rangoX)
    // Evaluar el polinomio p en el rango dado
    valoresX = linspace(rangoX(1), rangoX(2), 100);
    valoresY = horner(p, valoresX);
    
    // Graficar la curva del polinomio y los puntos.
    plot(xNodos,yNodos,'.');
    plot(valoresX, valoresY, "r", "LineWidth", 2);
    xgrid();
    
    // Opcional: Títulos y etiquetas
    legend(["Datos originales", "Polinomio ajustado"]);
    title("Ajuste por Mínimos Cuadrados");
    xlabel("x");
    ylabel("y");
endfunction

// Ejemplo. Polinomio que pase por (1,-1),(2,-1),(3,7),(4,8).
// Resultado: p(x) = 22 -39.5x +19x² -2.5x³ (ver en Geogebra).
x = [1, 2, 3, 4];
y = [-1, -1, 7, 8];

p = metodoDiferenciasDivididas(x,y);
disp("Polinomio interpolante: ", p);

// Graficar el polinomio en el intervalo [0, 5].
graficarPolinomio(p, x, y, [0, 5]);
