library(rethinking)
data(rugged)
d = rugged

d$log_gdp = log(d$rgdppc_2000)
