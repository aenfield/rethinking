# grid approximation

n_values = 20
p_grid = seq(from=0, to=1, length.out=n_values)

prior = rep(1, n_values)  # uniform
#prior = ifelse(p_grid < 0.5, 0, 1)  # step
#prior = exp(-5 * abs(p_grid - 0.5))  # peaked, at 0.5

likelihood = dbinom(6, size=9, prob=p_grid) # six successes, n=9
unstd_posterior = likelihood * prior
posterior = unstd_posterior / sum(unstd_posterior)

plot(p_grid, posterior, type='b', xlab='prob. of water', ylab='posterior')


# quadratic approximation
library(rethinking)

globe.qa = map(
  alist(
    w ~ dbinom(9,p),
    p ~ dunif(0,1)
  ),
  data=list(w=6))

precis(globe.qa)

# compare quadratic approx w/ analytic/exact calculation
w = 6
n = 9
curve(dbeta(x, w+1, n-w+1), from=0, to=1)
# approximation curve, first by repeating vals and then via the S4 obj from precis
#curve(dnorm(x, 0.67, 0.16), lty=2, add=TRUE)
globe.qa_precis = precis(globe.qa)  # doesn't yet take w and n as params
curve(dnorm(x, globe.qa_precis@output[['Mean']], 
               globe.qa_precis@output[['StdDev']]), lty=2, add=TRUE)
