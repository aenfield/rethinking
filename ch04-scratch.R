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
mu.list = seq(from=140, to=160, length.out=2000)
sigma.list = seq(from=4, to=9, length.out=2000)
post = expand.grid(mu=mu.list, sigma=sigma.list)
post$LL = sapply(1:nrow(post), function(i) sum(dnorm(
                d2$height,
                mean = post$mu[i],
                sd = post$sigma[i],
                log = TRUE )))
post$prod = post$LL + dnorm(post$mu, mu_mean, mu_sigma, TRUE) + 
                      dunif(post$sigma, sigma_min, sigma_max, TRUE)
post$prob = exp(post$prod - max(post$prod))

# entire joint distribution
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

# sampling from the posterior
sample.rows = sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
sample.mu = post$mu[sample.rows]
sample.sigma = post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex=1, pch=16, col=col.alpha(rangi2, 0.1))

# marginal posterior densities of each param
# the red book says 'marginal' means 'averaging over the other params'
# DBDA says it means without regard for other params - this defn makes more sense to me 
dens(sample.mu, show.HPDI=0.89)
dens(sample.sigma, show.HPDI=0.89)
  
HPDI(sample.mu)
HPDI(sample.sigma)


# heights with quadratic approx, and map
flist = alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 = map(flist, data=d2)
# get Gaussian approximations for each param's marginal distribution
precis(m4.1)

# just for reference, rather than letting map start at a random location we can specify start locs
m4.1_with_start = map(flist, start=list(mu = mean(d2$height), sigma=sd(d2$height)), data=d2)
precis(m4.1_with_start)

# with a more informative prior for mu
m4.2 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)
precis(m4.2)

# and sample from a map fit
vcov(m4.1)
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

post = extract.samples(m4.1, n=1e4)
head(post)
precis(post)

# side note: here's what extract.samples does internally
library(MASS)
mvrnorm_post = mvrnorm(n=1e4, mu=coef(m4.1), Sigma=vcov(m4.1))
head(post)
precis(post)
