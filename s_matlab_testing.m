clear; clc;
n=100;
x= -1;
f = studenttpdf(x, -4.8, 2.5, 5.8);
lf = studenttlogpdf(-1, -2, 3, 1);
f1 = studenttpdf1(-1, -2, 3, 1);

x = studenttrnd(-4.8, 2.5, 5.8, [n, 1]);


function f = studenttpdf(x, mu, sigma, nu)
    numer = (nu / (nu + ((x - mu) / sigma)^2))^((nu + 1) / 2);
    f = numer / (sqrt(nu) * sigma * beta(nu / 2, 1 / 2));
end

function f = studenttlogpdf(x, mu, sigma, nu)
    numer = (nu / (nu + ((x - mu) / sigma)^2))^((nu + 1) / 2);
    f = numer / (sqrt(nu) * sigma * beta(nu / 2, 1 / 2));
    f = log(f);
end

function f = studenttpdf1(x, mu, sigma, nu)
    f = tpdf((x - mu) / sigma, nu);
end

function x = studenttrnd(mu, sigma, nu, M)
    y = trnd(nu, M);
    x = sigma * y + mu;
end
