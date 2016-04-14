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
# calling link w/o specifying new data makes it use the original data
mu = link(m5.3)
# summarize samples across cases - we get, from mu - which is 50 sets of 1000 results (for each state) - 
# one average mu value and two numbers that show the PI range, for each state 
mu.PI = apply(mu, 2, PI)

# simulate observations - again, like w/ link, we pass no new data so sim will use the orig data
divorce.sim = sim(m5.3, n=1e4)
divorce.PI = apply(divorce.sim, 2, PI)

# and plot, a few diff ways
# first, predicted vs. observed and the CI for each prediction
plot(mu.mean ~ d$Divorce, col=rangi2, ylim=range(mu.PI), xlab="Observed divorce", ylab="Predicted divorce")
abline(a=0, b=1, lty=2)
for (i in 1:nrow(d)) {
  lines(rep(d$Divorce[i],2), c(mu.PI[1,i], mu.PI[2,i]), col=rangi2)
}
mu.mean = apply(mu, 2, mean)

# cool - interactively lets us click points on a chart and it'll return - here - the state abbrev
# assoc w/ the closest point (the labels aren't actually added to the plot until we click all the locations
# and then press ESC to stop)
identify(x=d$Divorce, y=mu.mean, labels=d$Loc, cex=0.8)

# the scatter plot above makes it harder to see the amount of prediction error per state; here we do a 
# 'residual plot' that shows the mean prediction error per row (using order here to sort from lowest
# prediction error to highest)
# compute residuatls first
divorce.resid = d$Divorce - mu.mean
# order by amount of error
o = order(divorce.resid)
# plot
dotchart(divorce.resid[o], labels=d$Loc[o], xlim=c(-6,5), cex=0.6)
abline(v=0, col=col.alpha("black", 0.2))
for (i in 1:nrow(d)) {
  j = o[i] # which state, in order  
  lines(d$Divorce[j] - c(mu.PI[1,j], mu.PI[2,j]), rep(i,2))
  points(d$Divorce[j] - c(divorce.PI[1,j], divorce.PI[2,j]), rep(i,2), pch=3, cex=0.6, col="gray")
}

# 'simulating spurious assocation' (p134, bottom) - xreal influences both the outcome y and a spurious
# predictor xspur; multiple regression can (often) figure this out
N = 100
x_real = rnorm(N) # x_real is Gaussian with mean 0 and sd 1
x_spur = rnorm(N, x_real) # Gaussian with mean x_real
y = rnorm(N, x_real) # Gaussian with mean x_real
d = data.frame(y, x_real, x_spur)

# x_real influences both x_spur and y - we might mistake x_spur as a predictor for y, because it's
# correlated w/ y - pairs(d) shows this correlation
pairs(d)

# but if we include both vars in a multiple regression predicting y, the posterior mean for the assoc
# between y and x_spur is close to zero while the comparable mean for x_real is closer to one
m5.p134 = map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + bR*x_real + bS*x_spur,
    a ~ dnorm(0, 10),
    bR ~ dnorm(0, 1),
    bS ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.p134)
plot(precis(m5.p134)) # should work, I think?

# Masked relationships
data(milk)
d = milk
str(d)

# first, simply see how milk energ level content is related to the relative amount of the brain mass
# taken up by the neocortex
m5.5 = map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data=d
)

# doesn't work as is because neocortex.perc has some NA values, and the likelihood of an NA is 
# undefined; we'll only use observations w/o NAs
d$neocortex.perc
dcc = d[complete.cases(d),]

m5.5 = map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data=dcc
)
precis(m5.5, digits=3)

# the mean of bn is small: 0.005, but we don't want to ignore it immediately because we're working with
# a percent and a small percent of large numbers can still be meaningful. That said, a change frome
# the largest to smallest neocortex percent in the data still only results in a change of less than
# 0.1 calories:
coef(m5.5)["bn"] * (max(d$neocortex.perc, na.rm=TRUE) - min(d$neocortex.perc, na.rm=TRUE))
# further, the 89% CI overlaps w/ zero and isn't precise - it's pretty big
# plot the predicted mean and 89% interval to see this more easily
np.seq = 0:100
pred.data = data.frame(neocortex.perc = np.seq)

mu = link(m5.5, data=pred.data, n=1e4)
mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)
# this shows that the MAP line is weakly positive, but that the PI includes a lot of both positive
# AND negative slopes

# let's try another predictor: adult female body mass; since 'scaling measurements like body mass are
# often related by magnitudes (not absolute values?) to other variables', we'll transform the body 
# mass to log(body mass) before we use it - i.e., we're saying that we want to see to what degree the
# _magnitude_ of the body mass is related to milk energy, and saying that we think the reln between 
# the magnitude and energy is linear.
dcc$log.mass = log(dcc$mass)

m5.6 = map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm*log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data=dcc
)
precis(m5.6, digits=3)

# plot expected/predicted ranges
np.seq = seq(from=min(dcc$log.mass), to=max(dcc$log.mass), length.out = 30)
pred.data = data.frame(log.mass = np.seq)

mu = link(m5.6, data=pred.data, n=1e4)
mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data = dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)
# so, the reln between log mass/mass magnitude and milk energy is weakly negative, but like with 
# neocortex percent, there are a wide range of possible slopes, including at least some slopes that
# are positive (if only very slightly)

# now let's try a multivariate regression with both variables in the same model
m5.7 = map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc + bm*log.mass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ), data=dcc
)
precis(m5.7, digits=3)

# how does milk energy vary by neocortex percent? this is counterfactual; we'll use the mean log body
# mass in this calculation (below, we'll swap)
mean.log.mass = mean(dcc$log.mass)
np.seq = 0:100
pred.data = data.frame(
  neocortex.perc = np.seq,
  log.mass = mean.log.mass
)

mu = link(m5.7, data=pred.data, n=1e4)
mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data=dcc, type="n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

# and now the other counter-factual plot: how does milk energy vary by the magnitude of body mass? 
# we'll use the mean neocortex percent.
mean.neocortex.perc = mean(dcc$neocortex.perc)
np.seq = seq(from=min(dcc$log.mass), to=max(dcc$log.mass), length.out = 30)
pred.data = data.frame(
  neocortex.perc = mean.neocortex.perc,
  log.mass = np.seq
)

mu = link(m5.7, data=pred.data, n=1e4)
mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI)

plot(kcal.per.g ~ log.mass, data=dcc, type="n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

# the description at the bottom of p140 does i think a good job of explaining how this masking works:
# here, both explanatory variables are correlated w/ the outcome (one postively and one negatively), and
# both explantory variables are _also_ correlated w/ each other - as a result, they cancel each other out.
# multivariate regression here teases this apart, because it answers these questions:
# - do species that have high neocortex percent _for their body mass_ have high milk energy?
# - do species that have high body mass _for their neocortex percent_ have high milk energy?

# the top of p141 has an overthinking that simulates another masking relationship

# TODO start at bottom of p141, 'When adding variables hurts' 