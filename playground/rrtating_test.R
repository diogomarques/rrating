source("R/rrating.R")

# set params
n = 1000
mean = 3
scale.from = 1
scale.to = 10

# generate distributions
responses.none = rrating(n = n, mean = mean, scale.from = scale.from,
                         scale.to = scale.to,
                         shift.to.mean = "none")
responses.quick = rrating(n = n, mean = mean, scale.from = scale.from,
                          scale.to = scale.to,
                          shift.to.mean = "quick")
responses.iterative = rrating(n = n, mean = mean, scale.from = scale.from,
                          scale.to = scale.to,
                          shift.to.mean = "iterative")
# compare

all = data.frame(none=responses.none, quick=responses.quick, iterative = responses.iterative)
means = all %>% summarise_each(funs(mean)) %>% mutate(stat="mean") %>% select(stat, none:iterative)
sds = all %>% summarise_each(funs(sd))  %>% mutate(stat="sd") %>% select(stat, none:iterative)
mins = all %>% summarise_each(funs(min))  %>% mutate(stat="min") %>% select(stat, none:iterative)
maxs = all %>% summarise_each(funs(max))  %>% mutate(stat="sd") %>% select(stat, none:iterative)
rbind(means , sds, mins, maxs)


# plot
table.responses.none = table(responses.none)
table.responses.quick = table(responses.quick)
table.responses.iterative = table(responses.iterative)
dimnames(table.responses.quick) = list(as.numeric(dimnames(table.responses.quick)[[1]]) + 0.05)
dimnames(table.responses.iterative) = list(as.numeric(dimnames(table.responses.iterative)[[1]]) + 0.1)
top = max(c(table.responses.quick, table.responses.none, table.responses.iterative))
plot(table.responses.none, ylim = c(0, top))
#abline(v = mean(responses.none), lty = 2)
axis(3, at = mean(responses.none), label = "", lwd = 3)
points(table.responses.quick, col = "red")
#abline(v = mean(responses.quick), lty = 2, col = "red")
axis(3, at = mean(responses.quick) + 0.05, label = "", col = "red", lwd = 3)
points(table.responses.iterative, col = "blue")
axis(3, at = mean(responses.iterative) + 0.10, label = "", col = "blue", lwd = 3)

