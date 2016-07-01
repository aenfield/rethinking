library(rethinking)
data(chimpanzees)
d = chimpanzees

head(d)

# clean up NAs from the data, since map2stan won't work with them
d2 = d
d2$recipient <- NULL


# first, just a simple model w/ an intercept - see just below for proof (or not?)
# that this is the same as a model w/o a linear term
m10.1 = map(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a,
    a ~ dnorm(0,10)
  ),
  data=d2
)
m10.1precis = precis(m10.1)
m10.1precis
# a (the intercept) is on the log-odds scale, so we need to use the inverse link
# function, which is the logistic function here, to convert them to probabilities
# a's 5.5% (index 3) is 0.176 log odds, the 94.5% (index 4) is 0.464 log odds.
# mean is index 1
logistic(m10.1precis@output[1])
logistic(as.numeric(c(m10.1precis@output[3], m10.1precis@output[4])))
# the above shows the MAP probability of pulling the left lever is 0.58, and the
# 89% interval, on the probability scale is 0.544 to 0.614.
# the MAP probability is the same as mean(d$pulled_left) - i think all this simple
# model is doing is saying that the most likely probability of pulling the left
# lever is 0.58, based on the data, with an 89% credible interval for the probability
# ranging between 0.544 and 0.614.

# my addition: I think the model above is the same as the very simple model where
# we have no linear equation component, and we won't have to do anything w/ logit 
# in the model and logistic to convert values back to probabilities we can understand.
# (the reason for the linear model is that it's what gives us the ability to
# incorporate predictor variables, but for this simple model we're not incorporating
# any - we have this model so we can show converting from log odds to probability
# in the simplest case)
m10.1b = map(
  alist(
    pulled_left ~ dbinom(1,a),
    a ~ dnorm(0,1)
  ),
  data=d2
)
precis(m10.1b)
# yep - the mean and credible interval for this simple model matches what we get
# by calling logistic on the output from the previous m10.1 model.
# finally, note that this means that the chimps had a preference for the left lever,
# generally


# now let's fit two other models that actually have predictor variables. I think
# this will let us evaluate to what extent particular other variables correlate
# with the pulled_left outcome, and, since we have multiple models, how well different
# kinds of relationships between pulled_left and predictor vars work.

# first
# predict using only prosoc_left (if the left hand lever was attached to the 
# prosocial option - the option where both chimps, if there are two, get food)
m10.2 = map(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a + bp*prosoc_left,
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10)
  ),
  data=d
)

# also
# predict using both prosoc_left and condition (1 when there's another chimp), with
# an interaction effect that says the impact of prosoc_left (on pulled_left being 1)
# depends on the value of condition (whether there's another chimp)
m10.3 = map2stan(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- a + (bp * bpC*condition)*prosoc_left,
    a ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  ),
  data=d
)

compare(m10.1, m10.1b, m10.2, m10.3)
