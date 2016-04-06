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

