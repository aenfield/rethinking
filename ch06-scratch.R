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

precis.plot(milk.models, SE=TRUE, dSE=TRUE)

coeftab(m6.11, m6.12, m6.13, m6.14)
plot(coeftab(m6.11, m6.12, m6.13, m6.14))

# model averaging

# compute counterfactual predictions
# neocortex from 0.5 to 0.8
nc.seq = seq(from=0.5,to=0.8,length.out=30)
d.predict = list(
  kcal.per.g = rep(0,30), # empty outcome
  neocortex = nc.seq,     # sequence of neocortex
  mass = rep(4.5,30)      # average mass
)
pred.m6.14 = link(m6.14, data=d.predict)
mu = apply(pred.m6.14, 2, mean)
mu.PI = apply(pred.m6.14, 2, PI)

plot(kcal.per.g ~ neocortex, d, col=rangi2)
lines(nc.seq, mu, lty=2)
lines(nc.seq, mu.PI[1,], lty=2)
lines(nc.seq, mu.PI[2,], lty=2)

milk.ensemble = ensemble(m6.11, m6.12, m6.13, m6.14, data=d.predict)
mu = apply(milk.ensemble$link, 2, mean)
mu.PI = apply(milk.ensemble$link, 2, PI)
lines(nc.seq, mu)
shade(mu.PI, nc.seq)
