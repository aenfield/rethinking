library(rethinking)
data(reedfrogs)
d = reedfrogs

# make the tank (i.e., each row) the cluster variable
d$tank = 1:nrow(d)

# fit model w/o pooling - i.e., this isn't yet a multilevel/hierarchical model
# here we just estimate, separately, an intercept (avg survival probability?)
# for each tank
m12.1 = map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(0, 5)
  ),
  data=d
)

# look at indiv estimates for each tank
results = precis(m12.1, depth=2)
results

# and to get the survival probability for a tank, use the logistic transform
log_odds_for_row_45_i_think = results@output[45,'Mean']
log_odds_for_row_45_i_think
logistic(log_odds_for_row_45_i_think)

# now let's do a model that uses the data from each tank to inform a shared
# prior - this _is_ a multilevel model
m12.2 = map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data=d, iter=4000, chains=4,
)

# now we have 50 params - we add the shared a and sigma params
precis(m12.2, depth=2)
compare(m12.1, m12.2)
