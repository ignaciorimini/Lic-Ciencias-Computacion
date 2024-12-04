function y = f4(x)
    y = log(x) - x + 1;
endfunction

x = 0.1:0.01:3;
plot(x, f4(x));
xlabel("x");
ylabel("y");
xtitle("Gráfico de f4(x)");
xgrid();

// Según el gráfico la raíz está cerca de x = 1.
// Según métodO de la secante:
// deff("y = p(x)", "y = log(x)-x+1");
// res = secante(u,0.9,1.1,0.000001,100); -> 1.0008830
