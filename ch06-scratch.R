library(rethinking)

data(milk)
d = milk[complete.cases(milk),]
d$neocortex = d$neocortex.perc / 100
dim(d)

a.start = mean(d$kcal.per.g)
sigma.start = log(sd(d$kcal.per.g))

m6.11 = map(
  alist(
    kcal.per.g ~ dnorm(a, exp(log.sigma))
  ), data=d, start=list(a=a.start, log.sigma=sigma.start)
)
precis(m6.11)
#m6.11_nolog = map(
#  alist(
#    kcal.per.g ~ dnorm(a, sigma)
#  ), data=d, start=list(a=a.start, sigma=sd(d$kcal.per.g))
#)
#precis(m6.11_nolog)

m6.12 = map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex
  ), data=d, start=list(a=a.start, bn=0, log.sigma=sigma.start)
)
precis(m6.12)

m6.13 = map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bm*log(mass)
  ), data=d, start=list(a=a.start, bm=0, log.sigma=sigma.start)
)
precis(m6.13)

m6.14 = map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex + bm*log(mass)
  ), data=d, start=list(a=a.start, bn=0, bm=0, log.sigma=sigma.start)
)
precis(m6.14)


WAIC(m6.14)

(milk.models <- compare(m6.11, m6.12, m6.13, m6.14))

plot(milk.models, SE=TRUE, dSE=TRUE)
