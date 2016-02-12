source("R/rrating.R")
n = 1000
mean = 3.5
mean = 4
mean = 2.1
sd = 10/4
scale.from = 1
scale.to = 10
shift.to.mean = "none"
shift.to.mean = "quick"

responses.none = rrating(n = n, mean = mean, scale.from = scale.from,
                         scale.to = scale.to, sd.norm = sd,
                         shift.to.mean = "none")
mean(responses.none)
sd(responses.none)



responses.quick = rrating(n = n, mean = mean, scale.from = scale.from,
                          scale.to = scale.to, sd.norm = sd,
                          shift.to.mean = "quick")
mean(responses.quick)
sd(responses.quick)
table.responses.quick = table(responses.quick)
dimnames(table.responses.quick) = list(as.numeric(dimnames(table.responses.quick)[[1]]) + 0.05)
dimnames(table.responses.quick)


plot(table(responses.none))
abline(v = mean(responses.none), lty = 2)
axis(3, at = mean(responses.none), label = "mean - none")
points(table.responses.quick, col = "red")
abline(v = mean(responses.quick), lty = 2, col = "red")
axis(1, at = mean(responses.quick), label = "mean - quick", col = "red")
