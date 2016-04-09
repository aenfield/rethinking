library(rethinking)

# grid approx for water example
w = 6; n = 9
p_grid = seq(from = 0, to = 1, length.out = 100)
posterior = dbinom(w, n, p_grid) * dunif(p_grid, 0, 1)
posterior = posterior / sum(posterior)
plot(posterior)
# dens(posterior) doesn't make sense - it's a histogram of values, not the dist itself

samples = sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
plot(samples)
dens(samples)
