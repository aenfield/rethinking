# very simple Metropolis implementation
num_weeks = 1e5
positions = rep(0, num_weeks)
current = 10

for (i in 1:num_weeks) {
  # record current position
  positions[i] = current
  
  # flip coin and generate proposal
  proposal = current + sample(c(-1,1), size=1)
  # handle wrap-around
  if (proposal < 1) proposal = 10
  if (proposal > 10) proposal = 1
  
  # move?
  prob_move = proposal / current
  current = ifelse(runif(1) < prob_move, proposal, current)
}
plot(1:125, positions[1:125])
hist(positions)


# map2stan intro
library(rethinking)
data(rugged)
d = rugged
d$log_gdp = log(d$rgdppc_2000)
dd = d[complete.cases(d$rgdppc_2000),]

# remember how we did this with 
m8.1 = map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data=dd
)
precis(m8.1)

# and w/ map2stan - only change is sigma prior and use of a data frame that
# has only the data the model needs (not required, but can resolve other problems,
# like NA data in non-used fields causing failures)
dd.trim = dd[, c("log_gdp","rugged","cont_africa")]
  
m8.1stan = map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2)
  ),
  data=dd.trim
)
precis(m8.1stan)

# draw samples again, using compiled model - four chains using four cores
# (I think each chain uses the default 1000 samples, so we get 4000 here vs
# 1000 above)
m8.1stan_4chains = map2stan(m8.1stan, chains=4, cores=4)
precis(m8.1stan_4chains)
# the resample function computes DIC and WAIC using new samples, you can pass
# the cores argument to the original map2stan call also.

# visualization
post = extract.samples(m8.1stan)
str(post)

pairs(post)

# use the fit model so R will automatically draw parameter names and
# numeric correlations
pairs(m8.1stan)

# use the samples
show(m8.1stan)
stancode(m8.1stan)
DIC(m8.1stan)
WAIC(m8.1stan)

# check the chain, diagnostics
plot(m8.1stan)

# 8.4.3 - taming a wild chain
# some posterior distributions - like those w/ 'broad, flat' regions - are hard to sample
# you can see this w/ flat priors, sometimes, for example and w/ little data like here:
y = c(-1, 1)
m8.2 = map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha
  ),
  data=list(y=y), start=list(alpha=0,sigma=1),
  chains=2, iter=4000, warmup=1000
)
precis(m8.2)
plot(m8.2)

# the above example is easily fixed by weakly informed priors - using these priors keeps
# the chain from having to sample all over the place, and including some crazy high or low
# priors (since flat/uniform priors are saying all values are equally plausible)
m8.3 = map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha,
    alpha ~ dnorm(1, 10),
    sigma ~ dcauchy(0, 1)
  ),
  data=list(y=y), start=list(alpha=0,sigma=1),
  chains=2, iter=4000, warmup=1000
)
precis(m8.3)
plot(m8.3)
# the above mean and sigma are as expected for -1 and 1 data (mu has a mean around zero, and
# a std dev around 1.4, both of which match the data); the priors are incredibly weak - 
# they're swamped by just two pieces of data - but w/o the priors we get bad data as m8.2 shows

# overthinking - the Cauchy distribution
# a very thick-tailed distribution, which means that at any point an extreme value - which is 
# at least somewhat likely given the thick tails - can overwhelm the previously drawn values,
# and therefore make the mean and std dev jump... the Cauchy distribution doesn't have a 
# stable mean or std dev.
y = rcauchy(1e4, 0, 5)
mu = sapply(1:length(y), function(i) sum(y[1:i])/i)
dev.off() # reset par
plot(mu, type='l')
# 'half Cauchy' means the distribution is defined, for ex, over positive reals only; you can
# see this in the stancode(m8.3) w/ the defined sigma variable and 'lower' (see p260 too)

# non-identifiable parameters
y = rnorm(100, mean=0, sd=1)

# non-identifiable params - alpha 1 and alpha 2 - with flat priors (and remember, 'flat priors
# are flat to infinity')
m8.4 = map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1 + a2,
    sigma ~ dcauchy(0,1)
  ),
  data=list(y=y), start=list(a1=0,a2=0,sigma=1),
  chains=2, iter=4000, warmup=1000, cores=4
)
precis(m8.4)
plot(m8.4)

# and with very weakly informed priors
m8.5 = map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1 + a2,
    a1 ~ dnorm(0,10),
    a2 ~ dnorm(0,10),
    sigma ~ dcauchy(0,1)
  ),
  data=list(y=y), start=list(a1=0,a2=0,sigma=1),
  chains=2, iter=4000, warmup=1000, cores=4
)
precis(m8.5)
plot(m8.5)
