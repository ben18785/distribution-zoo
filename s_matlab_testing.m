clear; clc;
n=100;
x= -1;
f = studenttpdf(x, -4.8, 2.5, 5.8);
lf = studenttlogpdf(-1, -2, 3, 1);

x = studenttrnd(-4.8, 2.5, 5.8, [n, 1]);
f = cauchypdf(-3, 10, 1);

x = cauchyrnd(100, 1, [10000,1]);

function f = studenttpdf(x, mu, sigma, nu)
    numer = (nu / (nu + ((x - mu) / sigma)^2))^((nu + 1) / 2);
    f = numer / (sqrt(nu) * sigma * beta(nu / 2, 1 / 2));
end

function f = studenttlogpdf(x, mu, sigma, nu)
    numer = (nu / (nu + ((x - mu) / sigma)^2))^((nu + 1) / 2);
    f = numer / (sqrt(nu) * sigma * beta(nu / 2, 1 / 2));
    f = log(f);
end

function x = studenttrnd(mu, sigma, nu, M)
    y = trnd(nu, M);
    x = sigma * y + mu;
end

function f = cauchypdf(x, a, b)
    f = b ./ (pi * (b.^2 + (x - a).^2));
end

function x = cauchyrnd(a, b, M)
    y = trnd(1, M);
    x = b * y + a;
end
