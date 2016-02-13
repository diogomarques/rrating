source("R/rrating.R")

# set params
n = 1000
mean = 3.5
mean = 2.5
mean = 2
sd = 10/5
scale.from = 1
scale.to = 10
shift.to.mean = "none"
shift.to.mean = "quick"

# generate distributions
responses.none = rrating(n = n, mean = mean, scale.from = scale.from,
                         scale.to = scale.to, sd.norm = sd,
                         shift.to.mean = "none")
responses.quick = rrating(n = n, mean = mean, scale.from = scale.from,
                          scale.to = scale.to, sd.norm = sd,
                          shift.to.mean = "quick")
# compare
mean(responses.none)
sd(responses.none)
mean(responses.quick)
sd(responses.quick)



# plot
table.responses.quick = table(responses.quick)
table.responses.none = table(responses.none)
dimnames(table.responses.quick) = list(as.numeric(dimnames(table.responses.quick)[[1]]) + 0.05)
top = max(c(table.responses.quick, table.responses.none))
plot(table.responses.none, ylim = c(0, top))
abline(v = mean(responses.none), lty = 2)
axis(3, at = mean(responses.none), label = "", lwd = 3)
points(table.responses.quick, col = "red")
abline(v = mean(responses.quick), lty = 2, col = "red")
axis(3, at = mean(responses.quick), label = "", col = "red", lwd = 3)
