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

