library(rethinking)

# estimate mu and sigma for height data
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
mu.list = seq(from=140, to=160, length.out=1000)
sigma.list = seq(from=4, to=9, length.out=1000)
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

# plotting regression intervals and contours - i.e., a more common way of 
# showing the uncertainty in the a and b posteriors than the cloud of 
# regression lines previously
post = extract.samples(m4.3)

# calc distribution of mu values given x-axis/weight value of 50kg
# this dist - say, the 89% HPDI - gives us the range of the interval at the
# specific x-axis value of 50
mu_at_50 = post$a + post$b * 50
dens(mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50", show.HPDI = 0.89)
HPDI(mu_at_50, prob=0.89)

# we want to calc dist of mu at _each_ x-axis value, not just 50kg
# we can then use all of the values to draw the interval for the entire range
# of x-axis values

# we use the link function which, by default, generates a matrix that has 1000
# rows (the default number of random samples) and 352 columns (one for each value
# in the source data d2). each column is 1000 values of mu that are generated as 
# a result of the weight value for that person (since weight is used to define
# mu in the model) and the particular value of a and b for each of the 1000 samples.
mu = link(m4.3)
str(mu)

# to draw the intervals we want something a bit different than the default return
# from the link function: we want a distribution of mu for each value of weight shown 
# on the x-axis; to get thsi we feed link different data using the 'data' param (the 
# doc says 'data' is an 'optional list of data to compute predictions over, when missing
# uses data found inside the fit object' - i.e., it uses each observation in the source data)
weight.seq = seq(from=25, to=70, by=1)
mu = link(m4.3, data=data.frame(weight=weight.seq))
str(mu)
# so, mu now has 1000 samples (of mean height) for each value of 'weight' from 25 to 70
# here's all 1000 values for the first weight value of 25
mu[,1]
# and all 1000 values for the last weight value of 75
mu[,46]

# use type="n" to hide the raw data - i.e., to just get the axes
# then plot each of the 1000 values we have for each of the 46 x-axis values
plot(height ~ weight, d2, type="n")
for (i in 1:1000)
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))

# summarize the distribution of mu - here we compute the mean of each column (dimension = '2')
# of the matrix mu; this gives us a vector of size 46 for mu.mean and 46*2 for mu.HPDI
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI, prob=0.89)

# and plot
# data
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
# MAP line
lines(weight.seq, mu.mean)
# shaded region for the 89% HPDI
shade(mu.HPDI, weight.seq)

# the code above shows the prediction interval for the _average height_, mu; we also want to 
# generate intervals for the actual heights
sim.height = sim(m4.3, data=list(weight=weight.seq), n=1e4) # 1e4 samples smooths out the PI interval
str(sim.height)
# so, we have a 1000x46 matrix, with a column of 1000 heights for each weight value

# and then calc the 89% interval for each weight value
height.PI = apply(sim.height, 2, PI, prob=0.89)

# plot raw data, then MAP line, then HDPI for line/mean mu, and finally PI value for simulated heights
plot(height ~ weight, d2, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
shade(height.PI, weight.seq)


# finish up w/ an intro to polynomial regression - now we'll use all observations, not just those of adults
d = Howell1
str(d)
plot(height ~ weight, data=d)

# standardize predictor to z-score representation, mainly to avoid numerical glitches that can come
# from very large numbers (which we can get when we square or cube)
d$weight.s = (d$weight - mean(d$weight)) / sd(d$weight)

# a parabolic model is almost identical to the non-polynomial model - we just add a square term to the
# calculation of mu
d$weight.s2 = d$weight.s ^ 2
m4.5 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=d)
precis(m4.5)

# it's harder to interpret especially the non-intercept coefficients a, b1, and b2, so we need to plot
weight.seq = seq(from=-2.2, to=2, length.out = 30)
pred_dat = list(weight.s = weight.seq, weight.s2 = weight.seq ^ 2)
mu = link(m4.5, data=pred_dat)
mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI, prob=0.89)
sim.height = sim(m4.5, data=pred_dat)
height.PI = apply(sim.height, 2, PI, prob=0.89)

plot(height ~ weight.s, data=d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# and for fun, try a cubic regression/higher-order polynomial using weight
d$weight.s3 = d$weight.s ^ 3
m4.6 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2 + b3*weight.s3,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ), data=d)
precis(m4.6)

# it's harder to interpret especially the non-intercept coefficients a, b1, and b2, so we need to plot
weight.seq = seq(from=-2.2, to=2, length.out = 30)
pred_dat = list(weight.s = weight.seq, weight.s2 = weight.seq ^ 2, weight.s3 = weight.seq ^ 3)
mu = link(m4.6, data=pred_dat)
mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI, prob=0.89)
sim.height = sim(m4.6, data=pred_dat)
height.PI = apply(sim.height, 2, PI, prob=0.89)

plot(height ~ weight.s, data=d, col=col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)

# and one way to convert standardized z-scores back to the natural scale: plot w/o any axis labels and 
# then manually set the axis labels using the natural values - this lets you fit the model and do 
# all of the plotting calculations (for intervals) in standardized units and then plot the estimates
# using the original scale; xaxt="n" turns off the horizontal axis
plot(height ~ weight.s, d, col=col.alpha(rangi2, 0.5), xaxt="n")
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)
at = c(-2, -1, 0, 1, 2)
labels = at*sd(d$weight) + mean(d$weight)
axis(side=1, at=at, labels=round(labels,1))
