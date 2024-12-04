// f1(x) = cos(x)cosh(x)+1
// cosh(x) = (e^x + e^-x)/2

function y = cosh(x)
    y = (exp(x) + exp(-x)) / 2;
endfunction

function y = f1(x)
    y = cos(x) .* cosh(x) + 1;
endfunction

// Se crea un vector de valores de x que van desde -2 hasta 2, con incrementos de 0.01. Este vector representará los puntos en el eje x donde se calculará la función.
x = -2:0.01:2;
plot(x, f1(x));
title("f1(x) = cos(x) cosh(x) + 1");
xlabel("x");
ylabel("y");
xtitle("Gráfico de f1(x)");
xgrid();


// Según el gráfico las raíces están cerca de x = -1.7 y x = 1.7.
// Veamos que arroja el métodO de la secante:
// deff("y = cosh(x)", "y = (exp(x)+exp(-x))/2");
// deff("y = h(x)", "y = cos(x) .* cosh(x) + 1");
// res = secante(h,-2,-1,0.0001,100); -> -1.8751096
// res = res = secante(h,1,2,0.0001,100); -> 1.8750976
