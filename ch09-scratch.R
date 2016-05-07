
# maximum entropy, with 10 pebbles
p = list()
p$A = c(0,0,10,0,0)
p$B = c(0,1,8,1,0)
p$C = c(0,2,6,2,0)
p$D = c(1,2,4,2,1)
p$E = c(2,2,2,2,2)
# Andrew added some more to see how they look/if they match the same line below
# i think i can do any permutation that sums to 10?
# actually... can't do this unless i also write a function that determines the 
# # of ways that you can get this particular set of #s. (i could use the regression
# line below to estimate the # of ways given the calculated info entropy, i think)
p$F = c(1,1,1,1,6)

# normalize to a prob distribution
p_norm = lapply(p, function(q) q/sum(q))

# and calc information entropy for each combination
(H = sapply(p_norm, function(q) -sum(ifelse(q==0,0,q*log(q)))))

# first calc # of ways each distribution can be realized, then divide by 10
# (the # of pebbles) to get the log ways per pebble for each distribution
ways = c(1,90,1260,37800,113400)
logwayspp
logwayspp = log(ways)/10

# and plot w/ regression line to show how information entropy (H) contains the 
# same information as log ways per pebble - info entropy is an approx  of log ways
# per pebble, and as the # of pebbles gets bigger, it becomes a better approx
# at just 10 pebbles it's already decent
plot(logwayspp, H)
abline(lm(H ~ logwayspp))

# keep going starting at 9.1.2
