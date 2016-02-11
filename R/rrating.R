# Function to generate random distributions of answers to Likert-type items
# answered on a numeric scale.
rrating = function(n, mean = 3, scale.from = 1, scale.to = 5,
                   sd.norm = 1, shift.to.mean = "none") {

  # TODO: default SD according to scale range

  # generate distribution, rounded to units
  dist = round(rnorm(n, mean = mean, sd = sd), digits = 0)

  # calculate breaks for normal distribution
  # TODO: break end points should take sd into consideration
  scale.range = scale.to - scale.from + 1
  lower.break = scale.from - 1000 * scale.range
  upper.break = scale.to + 1000 * scale.range
  breaks = c(lower.break, scale.from : (scale.to - 1), upper.break)
  breaks

  # remove extremes from distribution
  bounded = as.numeric(cut(dist, breaks = breaks, labels = c(scale.from:scale.to)))

  # if no shift to mean selected, return
  if(shift.to.mean == "none")
    return(bounded)


  # shift the distribution to one extreme w/ O(n) random shifts algorithm
  if(shift.to.mean == "quick") {

    # calculte how many shifts are needed
    shifts.left = round((mean - mean(bounded)) * n)

    # TODO: tidy up into single cycle
    while(shifts.left > 0) { # shift up
      # sample as many observations as possible at each round
      observations.not.at.top.end = length(which(bounded != scale.to))
      n.to.shift = min(shifts.left, observations.not.at.top.end)
      # shift
      observations.to.shift = sample(which(bounded != scale.to), n.to.shift)
      bounded[observations.to.shift] = bounded[observations.to.shift] + 1
      # recalculate n of shifts left
      shifts.left = round((mean - mean(bounded)) * n)
    }

    while(shifts.left < 0) { # shift down
      # sample as many observations as possible at each round
      observations.not.at.bottom.end = length(which(bounded != scale.from))
      n.to.shift = min(abs(shifts.left), observations.not.at.bottom.end)
      # shift
      observations.to.shift = sample(which(bounded != scale.from), n.to.shift)
      bounded[observations.to.shift] = bounded[observations.to.shift] - 1
      # recalculate n of shifts left
      shifts.left = round((mean - mean(bounded)) * n)
    }

    return(bounded)
  }

  # TODO: test alternatives: round-robin algorithm, iterative random
}






