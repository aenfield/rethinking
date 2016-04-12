library(rethinking)
data("WaffleDivorce")
d = WaffleDivorce

d$MedianAgeMarriage.s = (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)
d$Marriage.s = (d$Marriage-mean(d$Marriage))/sd(d$Marriage)

m5.1 = map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), data = d
)
precis(m5.1)

# percentile interval of the mean
MAM.seq = seq(from=-3, to=3.5, length.out = 30)
mu = link(m5.1, data=data.frame(MedianAgeMarriage.s=MAM.seq))
mu.PI = apply(mu, 2, PI)

# plot everything
plot(Divorce ~ MedianAgeMarriage.s, data=d, col=rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

sd(d$MedianAgeMarriage)

# multivariate models can often do a better job of teasing apart the actual influence of different
# variables - you won't see the right thing in some cases if you just do a set of bivariate regressions
# a multivariate regression answers the question: "what is the predictive value of a variable, once
# i already know all of the other predictor variables?"
# for ex, a single multivariate regression w/ our divorce data answers both of these questions:
# 1. after i already know marriage rate, what addtl value is there in knowing age at marriage?
# 2. after i already know age at marriage, what addtl value is there in knowing marriage rate?
# this is sometimes referred to as "controlling for the effect of <this particular variable>" etc., but 
# the author on p123 makes a good argument for why this is confusing and not accurate - we shouldn't
# overload "control".
m5.3 = map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data = d
)
precis(m5.3)
plot(precis(m5.3))
# this shows 'once we know the median age at marriage for a state, there's little to no addtl 
# predictive power in knowing the rate of marriage in the state'... because the coefficient and
# credible interval for bR is small/straddles zero, while the coefficient for bA is definitely
# negative and doesn't come close to zero.

# START on p126