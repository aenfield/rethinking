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
DIC(m8.1stan)
WAIC(m8.1stan)

# check the chain, diagnostics
plot(m8.1stan)
