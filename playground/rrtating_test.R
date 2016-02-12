source("R/rrating.R")
n = 1000
mean = 3.5
mean = 4
mean = 2.1
sd = 1
scale.from = 1
scale.to = 5
shift.to.mean = "none"
shift.to.mean = "quick"

par(mfrow=c(2,1))

responses.none = rrating(n = n, mean = mean, scale.from = 1, scale.to = 5, sd.norm = sd, shift.to.mean = "none")
plot(table(responses.none))
mean(responses.none)
sd(responses.none)

responses.quick = rrating(n = n, mean = mean, scale.from = 1, scale.to = 5, sd.norm = sd, shift.to.mean = "quick")
plot(table(responses.quick))
mean(responses.quick)
sd(responses.quick)

