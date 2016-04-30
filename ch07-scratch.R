library(rethinking)
data(rugged)
d = rugged

d$log_gdp = log(d$rgdppc_2000)

# only use countries w/ GDP data
dd = d[complete.cases(d$rgdppc_2000), ]

# split into Africa/not Africa
d.A1 = dd[dd$cont_africa==1, ]
d.A0 = dd[dd$cont_africa==0, ]

# fit and plot a model w/o interactions - a simple linear model
# African countries
m7.1 = map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=d.A1
)
precis(m7.1)

# non-African countries
m7.2 = map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=d.A0
)
precis(m7.2)

# plot both
calc_and_plot_MAP_and_PI_range = function(d, s, mu, formula) {
  mu.mean = apply(mu, 2, mean)
  mu.PI = apply(mu, 2, PI)
  
  plot(formula, data=d, col=rangi2)
  lines(s, mu.mean)
  shade(mu.PI, s)
}

r.seq = seq(from=-1,to=7, length.out = 30)

mu = link(m7.1, data=data.frame(rugged=r.seq))
calc_and_plot_MAP_and_PI_range(d.A1, r.seq, mu, log_gdp ~ rugged)

mu = link(m7.2, data=data.frame(rugged=r.seq))
calc_and_plot_MAP_and_PI_range(d.A0, r.seq, mu, log_gdp ~ rugged)

# we don't want to need to know to, or to actually, split data a priori
# first, let's try adding a dummy variable (peak: it won't work)
m7.3 = map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=dd
)
precis(m7.3)
  
m7.4 = map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=dd
)
precis(m7.4)

# compare using WAIC the above two models - one that gives us a line based only on ruggedness (and all
# of the data), and one that gives us two lines (with the same slope) based on ruggedness and
# on the dummy variable for in/out of Africa
compare(m7.3, m7.4)
# the results show that m7.4 gets all of the weight; the dSE (standard error of the difference)
# is 15 and the difference itself is 63, so a 95% interval is 63 +/- 30. bottom line: the 
# continent predictor is picking up a real difference, even accounting for overfitting.

# let's plot the posterior predictions for m7.4, so we can see that it doesn't actually give us
# different slopes in/out of Africa
rugged.seq = seq(from=-1, to=8, by=0.25)

# compute mu over samples, fixing cont_africa = 0
mu.NotAfrica = link(m7.4, data=data.frame(cont_africa=0, rugged=rugged.seq))
# compute mu over samples, fixing cont_africa = 1
mu.Africa = link(m7.4, data=data.frame(cont_africa=1, rugged=rugged.seq))

# same slope, just moved up/down 
calc_and_plot_MAP_and_PI_range(dd, rugged.seq, mu.NotAfrica, log_gdp ~ rugged)
calc_and_plot_MAP_and_PI_range(dd, rugged.seq, mu.Africa, log_gdp ~ rugged)
# overall, with all of the data we find a weak negative relationshp between ruggedness and
# log GDP; African countries have a lower overall GDP so the Africa line is lower - all that
# including the dummy Africa predictor has done is let the model predict a lower overall mean
# for African countries - it can't and doesn't do anything about the slope of the line, which
# the model defines as having to be the same for every country... i.e., the slope can't
# change dependent on country


# including an interaction term DOES let the slope change dependent on country location
m7.5 = map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + gamma*rugged + bA*cont_africa,
    gamma <- bR + bAR*cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), data=dd
)
precis(m7.5)

# compare the new model w/ the interaction effect, using WAIC, w/ the previous two models
compare(m7.3, m7.4, m7.5)
# m7.5 - the new one - gets 97% of the weight; the 3% given to m7.4 'indicates that the 
# posterior means for the slopes in m7.5 are a little overfit; the dSE in WAIC between the top
# two models is almost the same as the difference itself: because we don't have a lot of
# African countries, we don't have a lot of data to estimate the interaction

# note that we don't need to explicitly define a 'gamma' variable on a separate line like
# we did above; instead we can just put everything into a single mu line, like this
# mu <- a + bR*rugged + bAR*rugged*cont_africa + bA*cont_africa,

# plot both
# first, African countries
mu.Africa = link(m7.5, data=data.frame(cont_africa=1, rugged=rugged.seq))
calc_and_plot_MAP_and_PI_range(dd[dd$cont_africa==1,], rugged.seq, mu.Africa, log_gdp ~ rugged)
# and then non-African countries
mu.NotAfrica = link(m7.5, data=data.frame(cont_africa=0, rugged=rugged.seq))
calc_and_plot_MAP_and_PI_range(dd[dd$cont_africa==0,], rugged.seq, mu.NotAfrica, log_gdp ~ rugged)

# interactions are hard to interpret from just a table of means and std devs - instead it's
# better to plot implied predictions
# p221: "to get some idea of the uncertainty around gamma values, we need to use the whole
# posterior - since gamma depends on estimated parameters, and those parameters have a 
# posterior distribution, gamma must also have a posterior distribution."
# to calc the posterior distribution of gamma, either do calculus, or just use samples
# from the posterior:
post = extract.samples(m7.5)
gamma.Africa = post$bR + post$bAR*1
gamma.NotAfrica = post$bR + post$bAR*0

mean(gamma.Africa)
mean(gamma.NotAfrica)

# the above should be close to identical to the MAP values - i think this is what we'd calc
# if we figured the mean of bR and mean of bAR
precis(m7.5)
# since i don't remember off hand how to get data out of a precis result, I'll just calc manually
# Africa
-0.18 + 0.35*1
# not Africa
-0.18 + 0.35*0
# yep - they match

# but w/ the data from the samples, we actually have full posteriors and so can show the
# uncertainty, not just the MAP values - don't forget that these graphs are the marginal
# distributions - the overlap/relationship between the two doesn't hold, for that you typically
# need to actually calculate the differene using samples, and graph THAT resulting 
# distribution, as below these two lines...
dens(gamma.Africa, xlim=c(-0.5, 0.6), ylim=c(0, 5.5), xlab="gamma", col=rangi2)
dens(gamma.NotAfrica, add=TRUE)

# what's the probability, according to this model and data, that the slope within Africa is
# less than the slope outside Africa? We can't do this via the marginal distributions - instead
# we calculate the difference between the slopes for each sample in the posterior and use or 
# graph THAT distribution
diff = gamma.Africa - gamma.NotAfrica
sum(diff < 0) / length(diff)
# see the rethinking at the bottom of p222 for a good explanation of what this 0.003 probability
# actually means

# "Buridan's interaction" - symmetry of linear interaction
# where above we were looking at the impact of ruggedness on GDP, depending on whether the 
# country is in Africa, we can also - equally validly - look at the impact of being in/out of
# Africa on GDP, depending on ruggedness

# we'll graph for the min and max values of ruggedness, as the extremes through which other
# values of ruggedness lie.
q.rugged = range(dd$rugged)

# compute lines and CIs
mu.ruggedlo = link(m7.5, data=data.frame(rugged=q.rugged[1],cont_africa=0:1))
mu.ruggedlo.mean = apply(mu.ruggedlo, 2, mean)
mu.ruggedlo.PI = apply(mu.ruggedlo, 2, PI)

mu.ruggedhi = link(m7.5, data=data.frame(rugged=q.rugged[2],cont_africa=0:1))
mu.ruggedhi.mean = apply(mu.ruggedhi, 2, mean)
mu.ruggedhi.PI = apply(mu.ruggedhi, 2, PI)

# split points at median
med.r = median(dd$rugged)
ox = ifelse(dd$rugged > med.r, 0.05, -0.05)
plot(dd$cont_africa + ox, log(dd$rgdppc_2000), 
     col=ifelse(dd$rugged > med.r, rangi2, "black"), 
     xlim=c(-0.25, 1.25), xaxt="n", ylab="log GDP year 2000",
     xlab="Continent")

axis(1, at=c(0,1), labels=c("other","Africa"))

lines(0:1, mu.ruggedlo.mean, lty=2)
shade(mu.ruggedlo.PI, 0:1)
lines(0:1, mu.ruggedhi.mean, lty=2, col=rangi2)
shade(mu.ruggedhi.PI, 0:1, col=col.alpha(rangi2, 0.25))


# 7.3 - continuous interactions, where in the intro he explains another reason that interaction
# effects are difficult to interpret: continuous variable interactions are especially opaque
# (the example above is easier to interpret because one of the variables is categorical)
data("tulips")
d = tulips
str(d)

# first, show that it doesn't work well w/o centering
m7.6 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), data=d
)
m7.7 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade + bWS*water*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), data=d
)

# fix by using a few of the (more) recommended options on p228: use a different optimization
# method, and tell optim to search longer
m7.6 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data=d,
  method="Nelder-Mead",
  control=list(maxit=1e4)
)
m7.7 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade + bWS*water*shade,
    a ~ dnorm(0, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data=d,
  method="Nelder-Mead",
  control=list(maxit=1e4)
)

# we can't really interpret w/o plotting posterior predictions, but we'll still look at the #s
coeftab(m7.6, m7.7)

precis(m7.6)
precis(m7.7)

compare(m7.6, m7.7)

# center and re-estimate, and see if/how that changes anything - this fixes the need for more
# iterations and alternate optimization methods that we had to use above, and also makes it 
# easier to interpret the estimates. centering here changes the values from 1-3 to -1 to 1.
d$shade.c = d$shade - mean(d$shade)
d$water.c = d$water - mean(d$water)

m7.8 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water.c + bS*shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data=d,
  start=list(a=mean(d$blooms), bW=0, bS=0, sigma=sd(d$blooms))
)
m7.9 = map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c,
    a ~ dnorm(130, 100),
    bW ~ dnorm(0, 100),
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100)
  ), 
  data=d,
  # provide better starting methods because flat priors here provide 'terrible' random
  # starting locations (could also be better ? fixed i wonder by better priors?)
  start=list(a=mean(d$blooms), bW=0, bS=0, bWS=0, sigma=sd(d$blooms))
)

coeftab(m7.8, m7.9)
