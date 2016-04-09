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

# work now w/ height and weight - 'adding a predictor'
plot(d2$height ~ d2$weight)

m4.3 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), 
  data=d2
)
precis(m4.3, corr=TRUE)

# a and b are almost exactly negatively correlated w/ each other (-0.99) - ok, here as it just means
# that these params carry the same info (change one and the other changes to match it); it can make
# it difficult to fit more complicated models to data, so we can do things to help, like 'centering'...

# centering is subtracting the mean of a variable from each value. for weight...
d2$weight.c = d2$weight - mean(d2$weight)

m4.4 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), 
  data = d2
)
precis(m4.4, corr=TRUE)
mean(d2$weight.c)
# as p100 discusses, the intercept - in addition to meaning 'the expected value of the outcome variable,
# when the predictor is equal to zero' - also means 'the expected value of the outcome, when the
# predictor is at its average value'. 

# plot posterior inference against the data - is this a 'posterior predictive check' per DBDA?
# start w/ superimposing the MAP values for mean height over the actual data
plot(height ~ weight, data=d2)
abline(a = coef(m4.3)["a"], b=coef(m4.3)["b"])

# the MAP line is 'just the posterior mean - the most plausible line in the infinite universe of lines
# that have been considered in the posterior distribution'. That is, the line doesn't convey uncertainty -
# we could have many lines very much like the MAP line, or very few. Let's sample a bunch of lines
# (that is, values of a and b) and plot them.
# With fewer data points our estimates will be wider, so define a function that runs the model on a 
# specified number of data points and then plots a representative sample of the resulting lines.
filter_to_n_obs_and_plot = function(N) {
  dN = d2[1:N,]
  mN = map(
    alist(
      height ~ dnorm(mu, sigma),
      mu <- a + b*weight,
      a ~ dnorm(156, 100),
      b ~ dnorm(0, 10),
      sigma ~ dunif(0, 50)
    ), 
    data=dN
  )
  # extract 20 representative samples
  post = extract.samples(mN, n=20)
  # display raw data and sample size
  plot(dN$weight, dN$height,
       xlim=range(d2$weight), ylim=range(d2$height),
       col=rangi2, xlab="weight", ylab="height")
  mtext(concat("N = ", N))
  # plot the lines
  for (i in 1:20)
    abline(a = post$a[i], b=post$b[i], col=col.alpha("black", 0.3))
}
filter_to_n_obs_and_plot(10)
filter_to_n_obs_and_plot(20)
filter_to_n_obs_and_plot(50)
filter_to_n_obs_and_plot(150)
filter_to_n_obs_and_plot(length(d2$weight))
