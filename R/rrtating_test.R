source("R/rrating.R")
n = 1000
mean = 3
mean = 4
mean = 2.1
sd = 1
scale.from = 1
scale.to = 5
shift.to.mean = "none"
shift.to.mean = "quick"

par(mfrow=c(2,2))

responses.none = rrating(n = n, mean = mean, scale.from = 1, scale.to = 5, sd.norm = sd, shift.to.mean = shift.to.mean)
mean(responses.none)
sd(responses.none)
table(responses.none)
plot(table(responses.none))
