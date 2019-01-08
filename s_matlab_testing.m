clear; clc;
n=100;
x= -1;
f = studenttpdf(x, -4.8, 2.5, 5.8);
lf = studenttlogpdf(-1, -2, 3, 1);

x = studenttrnd(-4.8, 2.5, 5.8, [n, 1]);
f = cauchypdf(-3, 10, 1);

x = cauchyrnd(100, 1, [10000,1]);

f = halfcauchypdf(3, 10, 1);

% x = halfcauchyrnd(0, 1, [100000,1]);

f = inversegammapdf(5, 3, 2);
x = inversegammarnd(3, 2, [10000, 1]);


f = inversechisquaredpdf(3, 5);
x = inversechisquaredrnd(5, [10000, 1]);

f = logitnormalpdf(0.5, -1, 2);
x = logitnormalrnd(2, 2.1, [100000, 1]);

f = betabinomialpdf(15, 20, 15, 1);
x = betabinomialrnd(100, 10, 10, [10000, 2]);

f = multivariatetpdf([1, 2], [1, -3], [[2, 0.5]; [0.5, 0.9]], 1);
x = multivariatetrnd([1, -3], [[2, 0.5]; [0.5, 0.9]], 1, 100);

f = inversewishartpdf([[2, 0.5]; [0.5, 1]], 10, [[3, 0]; [0, 2]]);

f = lkjpdf(diag([1, 1]), 10);

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


function f = halfcauchypdf(x, a, b)
    if x < 0
        f = 0;
    else
        fun = @(y) b ./ (pi * (b.^2 + (y - a).^2));
        c = integral(fun, 0, Inf);
        f = (1 / c) * b ./ (pi * (b.^2 + (x - a).^2));
    end
end

function x = halfcauchysinglernd(a, b)
    x = cauchyrnd(a, b, 1);
    while x < 0
        x = cauchyrnd(a, b, 1);
    end
end

function x = halfcauchyrnd(a, b, M)
    x = zeros(M);
    n = numel(x);
    y = zeros([n, 1]);
    for i = 1:n
        y(i) = halfcauchysinglernd(a, b);
    end
    x = reshape(y, M);
end

function f = inversegammapdf(x, alpha, beta)
    if x < 0
        f = 0;
    else
        f = (beta^alpha) / gamma(alpha) * x^(-alpha-1) * exp(- beta / x);
    end
end

function x = inversegammarnd(alpha, beta, M)
    y = gamrnd(alpha, 1 / beta, M);
    x = 1 ./ y;
end

function f = inversechisquaredpdf(x, nu)
    f = 2^(-nu/2) / gamma(nu / 2) * x^(-nu / 2 - 1) * exp(-1 / (2 * x));
end

function x = inversechisquaredrnd(nu, M)
    y = chi2rnd(nu, M);
    x = 1 ./ y;
end

function f = logitnormalpdf(x, mu, sigma)
    if x > 1 || x < 0
        f = 0;
    else
        f = 1 / (sigma * sqrt(2 * pi)) * exp(-(log(x / (1 - x)) - mu)^2 / (2 * sigma^2));
    end
end

function x = logitnormalrnd(mu, sigma, M)
    y = normrnd(mu, sigma, M);
    x = 1 ./ (1 + exp(-y)); 
end

function f = betabinomialpdf(x, n, alpha, beta1)
    if abs(x - round(x)) > 0
        f = 0;
    elseif x < 0 || x > n
        f = 0;
    else
        f = nchoosek(n, x) * beta(x + alpha, n - x + beta1) / beta(alpha, beta1);
    end
end

function x = betabinomialrnd(n, alpha, beta1, M)
    theta = betarnd(alpha, beta1, M);
    x = binornd(n, theta);
end

function f = mvn2dpdf(x, mux, muy, sigmax, sigmay, rho)
    f = mvnpdf(x, [mux, muy], [[sigmax^2, sigmax * sigmay * rho]; [sigmax * sigmay * rho, sigmay^2]]);
end

function f = multivariatetpdf(x, mu, Sigma, nu)
    d = length(mu);
    x_minus_mu = reshape(x - mu, d, 1);
    f = gamma((nu + d) / 2) / (gamma(nu / 2) * nu^(d / 2) * pi^(d / 2) * det(Sigma)^0.5) * (1 + (1 / nu) * x_minus_mu' * inv(Sigma) * x_minus_mu)^(-(nu + d) / 2);
end

function x = multivariatetrnd(mu, Sigma, nu, n)
    d = length(mu);
    y = mvnrnd(zeros([d, 1]), Sigma, n);
    u = chi2rnd(nu, n);
    x = zeros([n, d]);
    for i = 1:n
        x(i, :) = mu + y(i, :) / sqrt(u(i) / nu);
    end
end

function g = multivariate_gamma(p, a)
    j = 1:p;
    g = gamma(a + (1 - j) / 2);
    g = pi^(p * (p - 1) / 4) * prod(g);
end

function f = wishartpdf(x, nu, S)
    m = size(x);
    d = m(1);
    if nu < d - 1
        f = 0;
    else
        f = det(x)^((nu - d - 1) / 2) * exp(-trace(inv(S) * x) / 2) * 1 / (2^(nu * d / 2) * det(S)^(nu / 2) * multivariate_gamma(d, nu / 2));
    end
end

function f = inversewishartpdf(x, nu, S)
    m = size(x);
    d = m(1);
    if nu < d - 1
        f = 0;
    else
        f = det(S)^(nu / 2) * det(x)^(-(nu + d + 1) / 2) * exp(-trace(S * inv(x)) / 2) * 1 / (2^(nu * d / 2) * multivariate_gamma(d, nu /2));
    end
end


function f = lkjpdf(x, nu)
    m = size(x);
    d = m(1);
    a_sum = 0;
    a_prod = 1;
    for k = 1:(d-1)
        a_sum = a_sum + (2 * nu - 2 + d - k) * (d - k);
        a_prod = a_prod * beta(nu + 0.5 * (d - k - 1), nu + 0.5 * (d - k - 1));
    end
    a_sum = 2^a_sum;
    f = a_sum * a_prod * det(x)^(nu - 1);
end