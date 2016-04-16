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

# 'When adding variables hurts' 

# experiment with simulated leg lengths and predicting height
N = 100
height = rnorm(N,10,2)
leg_prop = runif(N,0.4,0.5)
leg_left = leg_prop*height + rnorm(N,0,0.2)
leg_right = leg_prop*height + rnorm(N,0,0.2)
d = data.frame(height, leg_left, leg_right)

# we'd expect, absent multi-collinearity, for each coefficient to have a value of avg height - 10 - 
# divided by 45% of the avg height - 4.5, or 2.2.

m5.8 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10,100),
    bl ~ dnorm(2,10),
    br ~ dnorm(2,10),
    sigma ~ dunif(0,10)
  ), data=d
)
precis(m5.8)
plot(precis(m5.8))

# what's the bivariate posterior distribution for bl and br?
post = extract.samples(m5.8)
plot(bl ~ br, post, col=col.alpha(rangi2,0.1), pch=16)

# when br is large (and it can be large), bl has to be small, and vice-versa; since there are plausible
# values of br (and bl) that are large, and small, the posterior distribution for these two variables
# will be wide - for example, each CI will include both small and large values.

# another way to think about this is that rather than having a linear model with two different predictors
# and two different coefficients, you actually have a model/equation with two terms and two coefficients
# but only one actualy predictor - i.e., you have mu = alpha + b1x + b2x, which is the same as 
# mu = alpha + (b1+b2)x.

# we can actually find what we really want (and what we'd get if we only included a single leg) by 
# summing bl and br
sum_blbr = post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab="sum of bl and br")
mean(sum_blbr)
sd(sum_blbr)

# and if we just included a single predictor
m5.9 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left,
    a ~ dnorm(10,100),
    bl ~ dnorm(2,10),
    sigma ~ dunif(0,10)
  ), data=d
)
precis(m5.9)
plot(precis(m5.9))

# the mean and SD for bl here matches what we get when we sum bl+br above

# to summarize: the original model with both collinear predictors still predicts just as well as a
# model with only a single predictor (that carries the same info) - what we lose w/ the collinear
# predictors is the ability to infer anything about the relative importance of the two leg length
# variables.

# there's a real-world example of multi-collinearity with the milk data on p145-p150.

# the text on the bottom of p149 defines 'non-identifiability': a non-identifiable parameter, the 
# structure of the data and the model do not make it possible to estimate the parameter's value (well).
# for ex, when the available data don't contain much info about a parameter, you'll - as expected -
# get a very wide posterior distribution. there's more in these few paragraphs, which are good.

# post-treatment bias
# be careful to not include as predictors variables that are 'post-treatment' variables - those that
# are affected by other predictors. for ex, suppose we want to investigate the impact of soil treatments
# that reduce fungus on the height of plants. we SHOULD NOT include 'fungus yes/no' as a predictor
# because fungus changes as a result of the treatments (in theory) - instead, we should just include
# the treatments and the height. (this is harder to see in observational studies than in experiments
# like that simulated below.)

# let's simulate the above example
N = 100
h0 = rnorm(N,10,2) # initial height
# assign to treatments and simulate growth
treatment = rep(0:1, each=N/2)
fungus = rbinom(N, size=1, prob=0.5 - treatment*0.4) # existence of fungus: 50% no treatment, 10% with
h1 = h0 + rnorm(N, 5 - 3*fungus)
d = data.frame(h0, h1, treatment, fungus)

# first try, incorrectly - post-treatment bias - with fungus as a predictor
m5.13 = map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment + bf*fungus,
    a ~ dnorm(0,100),
    c(bh,bt,bf) ~ dnorm(0,10), # cool way to define multiple priors at the same time
    sigma ~ dunif(0,10)
  ), data=d
)
precis(m5.13)

# we see both fungus and treatment as having a _negative_ effect on height... we know treatment should
# have a postive effect because we built the simulated data to show this. what's happened is that w/ the
# model above we're asking 'given that we know if a plant has fungus, does treatment matter?' and 'given
# that we know a plant has been treated, does knowing if a plant has fungus matter?'. 
# we should instead leave out fungus, since it's a post-treatment variable.
m5.14 = map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment,
    a ~ dnorm(0,100),
    c(bh,bt) ~ dnorm(0,10), # cool way to define multiple priors at the same time
    sigma ~ dunif(0,10)
  ), data=d
)
precis(m5.14)
# now the effect of treatment is positive, as we truly expect/know.


# Categorical variables
# binary categories
data("Howell1")
d = Howell1
str(d)

m5.15 = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(178,100),
    bm ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ), data=d
)
precis(m5.15)
# so a is the mean height when you're a female, and a+bm is the avg height when you're a male

# adding a+bm gives us the posterior mean of avg male height; to get the range/width of the posterior
# you CANNOT just add the summary means and widths - because a and bm are correlated - so you instead
# need to pull actual samples of heights and then calculate the width from the samples. working w/ 
# samples automatically handles the correlation problem and works the same regardless of the degree
# of correlation
post = extract.samples(m5.15)
mu.male = post$a + post$bm
PI(mu.male)

# you could also 're-parameterize' - build the same model w/ different parameters - and have separate
# variables that hold the average male and female heights - note no intercept/a parameter
m5.15b = map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- af*(1-male) + am*male,
    c(af,am) ~ dnorm(178,100),
    sigma ~ dunif(0,50)
  ), data=d
)
precis(m5.15b)

# many categories
data(milk)
d = milk
str(d)
unique(d$clade)

# create dummy vars, one for n-1 values for the clade factor
(d$clade.NWM = ifelse(d$clade=='New World Monkey', 1, 0))
(d$clade.OWM = ifelse(d$clade=='Old World Monkey', 1, 0))
(d$clade.S = ifelse(d$clade=='Strepsirrhine', 1, 0))
# we don't include one for 'Ape' because that'd create a non-identifiable model, I think because then
# a/the intercept and clade.A would both be able to have the same effect and they'd trade off and both
# potentially have high values (when the other's value is low) and vice versa; instead we just have the
# intercept param that represents the Ape's milk energy.
m5.16 = map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S,
    a ~ dnorm(0.6, 10),
    c(b.NWM, b.OWM, b.S) ~ dnorm(0, 1),
    sigma ~ dunif(0,10)
  ), data=d
)
precis(m5.16)

# since a is the avg milk energy for 'Ape' and the estimates for the other categories are the 
# differences from apes, we need to use samples to get posterior distributions for the other classes
post = extract.samples(m5.16)
mu.ape = post$a
mu.NWM = post$a + post$b.NWM
mu.OWM = post$a + post$b.OWM
mu.S = post$a + post$b.S

# summarize using precis w/ a custom-created dataframe
precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S))

# side note: the samples have all of the info about uncertainty, and so you can 'effectively 
# re-parameterize' _after_ we've already fit the model. for ex, suppose we want the difference between
# the two monkey groups - just subtract the estimated means using the samples
diff.NWM.OWM = mu.NWM - mu.OWM
# and here are the lower 95%, the median, and the upper 95% - this is posterior distribution for the 
# difference between NWM and OWM - none of the uncertainty in the orig posterior is lost
quantile(diff.NWM.OWM, probs=c(0.025, 0.5, 0.975))

# there's a great 'differences and statistical significance' rethinking section at the bottom of p157:
# about how you can/can't reason w/ 'significant' and 'not significant' variables - you have to 
# compute a contrast/the difference and look at the distribution. it also makes cogently the point that
# 'we found no difference' or 'no effect', which in actuality means that some parameter was not
# significantly different from zero as evidence that the parameter IS zero - this is exactly 'accepting/
# proving the null' and not something we can do.

# i think i _could_ leave out the intercept param and replace it w/ an Ape-specific dummy var though
# and i can... and then i don't need to use samples and manually calc the means and ranges for all 
# four categories - the precis below matches the precis above
(d$clade.A = ifelse(d$clade=='Ape', 1, 0))
m5.16b = map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S + b.A*clade.A,
    c(b.NWM, b.OWM, b.S, b.A) ~ dnorm(0, 1),
    sigma ~ dunif(0,10)
  ), data=d
)
precis(m5.16b)

# finally, see the 'unique intercepts' approach below that uses Stan index variables to do the same thing
# as w/ my Ape example
(d$clade_id = coerce_index(d$clade))

m5.16_alt = map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)
precis(m5.16_alt, depth=2) # depth required to show the index/vector parameters
# yep - the results are the same as both of the different approaches above

# finish up ch5 by using the OLS lm, and map formulas from lm
# "Carl Friedrich Gauss himself invented OLS as a method of computing Bayesian MAP estimates"

# see p159-p161 for a bunch of examples

# glimmer function to translate lm's 'design formulas' to map-style formulas
data(cars)
formula = glimmer(dist ~ speed, data=cars)

m.mine = map(formula$f, data=formula$d)
precis(m.mine)
