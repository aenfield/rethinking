height = seq(from=0, to=9, length.out = 10)
weight = seq(from=0, to=9, length.out = 10)
d = data.frame(height, weight)

library(rethinking)

plot(d$height ~ d$weight)

m = map(
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

abline(a = coef(m)["a"], b=coef(m)["b"])
