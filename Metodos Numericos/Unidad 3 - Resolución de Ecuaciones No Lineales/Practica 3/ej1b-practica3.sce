function y = f2(x)
    y = 2*sin(x)-x^2;
endfunction

x = -1:0.01:2;
plot(x,f2(x));
title("f2(x)=2*sin(x)-x^2");
xlabel("x");
ylabel("y");
xtitle("Gráfico de f2(x)");
xgrid();

// Se puede observar en la gráfica que las raíces están cerca de x = 0 y de x = 1.4 aproximadamente. Verificar utilizando algún métod para encontrar raíces
// Según metodO de la secante:
// deff("y=f(x)", "y=2*sin(x)-x^2");
// res = secante(f,-1,0.5,0.00001,100); -> 0.0000021
// res = secante(f,1,2,0.00001,100); -> 1.4044113
