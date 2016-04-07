# grid approx for water example
w = 6; n = 9
p_grid = seq(from = 0, to = 1, length.out = 100)
posterior = dbinom(w, n, p_grid)dunif(p_grid, 0, 1)
posterior = posterior / sum(posterior)
dens(posterior)

# estimate mu and sigma for height data
library(rethinking)
data("Howell1")
d = Howell1

class(d)
str(d)

d$height

d2 = d[d$age >= 18, ]
NROW(d2)

mu_mean = 178
mu_sigma = 20
sigma_min = 0
sigma_max = 50
curve(dnorm(x, mu_mean, mu_sigma), from=100, to=250)
curve(dunif(x, sigma_min, sigma_max), from=-10, to=60)

n = 1e4
sample_mu = rnorm(n, mu_mean, mu_sigma)
sample_sigma = runif(n, sigma_min, sigma_max)
prior_h = rnorm(n, sample_mu, sample_sigma)
dens(prior_h)

# grid approx
mu.list = seq(from=)