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

# predictor residual plots
# shows 'the average prediction error when all other variables are used to model a predictor of 
# interest'. first we compute predicted values for a given predictor by 'using the other predictor(s)
# to model it' - for ex, we use median age at marriage to predict the marriage rate. Then, we
# compute residuals by subtracting the actual/observed marriage rate from the predicted rate (predicted
# based on the age at marriage).
m5.4 = map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.4)

# compute expected value at MAP, for each state
mu = coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s
# and then compute residual for each state
m.resid = d$Marriage.s - mu

plot(Marriage.s ~ MedianAgeMarriage.s, d, col=rangi2)
abline(m5.4)
# plot residual for each state
for (i in 1:length(m.resid)) {
  x = d$MedianAgeMarriage.s[i]  # x location of line segment
  y = d$Marriage.s[i] # observed endpoint of line segment
  # draw the line segment
  lines(c(x,x), c(mu[i],y), lwd=0.5, col=col.alpha("black", 0.7))
}

# there's more on these plots on p126 and p127, including code that plots the residuals against
# the overall predicted variable (divorce). these plots then show the effect of each predictor having
# statistically "controlled" for all of the other predictors. all that said, this is manual - we get
# the same thing for free if we just do multivariate regression.

# skipping for now at least the 5.1.3.2. counterfactual plot section

# 5.1.3.3. posterior predictive plots
# TBD p131