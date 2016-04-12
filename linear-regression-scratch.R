# first, with an exactly correlated set of explanatory and response vars
height = seq(from=0, to=9, length.out = 10)
weight = seq(from=0, to=9, length.out = 10)
d = data.frame(height, weight)

library(rethinking)

plot(d$height ~ d$weight)

# map appears to have numerical issues - perhaps because of zeros, etc. so we
# try map2stan w/ the identical formula input and - despite the warnings it shows
# it appears to work
m = map2stan(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), 
  data=d,
  start = list(a = 0, b = 0, sigma = 0.01)
)
precis(m, corr=TRUE)

plot(d$height ~ d$weight)
abline(a = coef(m)["a"], b=coef(m)["b"])

# now try explanatory vars that when averaged produce the same as above
height = c( 0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9)
weight = c(-1,1,0,2,1,3,2,4,3,5,4,6,5,7,6,8,7,9,8,10)
d = data.frame(height, weight)

plot(d$height ~ d$weight)

m = map2stan(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), 
  data=d,
  start = list(a = 0, b = 0, sigma = 0.01)
)
precis(m, corr=TRUE)

plot(d$height ~ d$weight)
abline(a = coef(m)["a"], b=coef(m)["b"])

# what does OLS regression produce?
ols_fit = lm(height ~ weight, data=d)
summary(ols_fit)

# it's the same as Bayesian... the initial apparent oddness w/ the line only 
# being exactly/close to halfway between the observations maybe has to do with
# how the bottom two and top two observations, as far as the x-axis goes
# (-1, 0, and 9, 10) are by themselves and don't have two observations w/ the same
# x-axis value. Let's try w/ slightly different data that always has paired x-axis
# values
height = c(0,2,1,3,2,4,3,5,4,6,5,7,6,8,7,9,8,10,9,11)
weight = c(0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8, 8,9, 9)
d = data.frame(height, weight)

plot(d$height ~ d$weight)

m = map2stan(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), 
  data=d,
  start = list(a = 0, b = 0, sigma = 0.01)
)
precis(m, corr=TRUE)

plot(d$height ~ d$weight)
abline(a = coef(m)["a"], b=coef(m)["b"])

# and, OLS again
ols_fit = lm(height ~ weight, data=d)
summary(ols_fit)
