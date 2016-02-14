#' Simulate Rating Scale Data
#'
#' Generates random answers to rating scales in surveys, that approximate a
#' normal distribution.
#'
#'
#' @param n number of observations.
#' @param mean mean of observations (actual mean will not coincide if
#' \code{shift.to.mean}) is set to "none".
#' @param scale.from bottom-end of the possible values in scale.
#' @param scale.to top-end of the possible values in scale.
#' @param mean.norm mean of the normal distribution to be approximated.
#' @param sd.norm standard deviation of the normal distribution to be
#' approximated.
#' @param shift.to.mean if set to "none", the mean of observations will
#' generally not match the mean of the normal to be approximated; to shift the
#' observations to match it, set to either "quick", "iterative", or
#' "round-robin" (algorithms discussed in the details)
#' @return A vector of n length.
#' @details TODO
#' @export
#' @examples
#' # Simulate 1000 responses for question "From 1 to 5, how much do you like pizza?"
#' responses = rrating(1000, mean = 4, scale.from = 1, scale.to = 5)
#' plot(table(responses))
#' mean(responses)
#' #
#' # mean is not 4, because tails of the normal distribution being approximated
#' # were binned with the boundaries. To keep mean at four, set the shift option:
#' responses.shifted = rrating(1000, mean = 4, scale.from = 1, scale.to = 5,
#' shift.to.mean = "quick")
#' plot(table(responses.shifted))
#' mean(responses.shifted)
#'
#'
#'
rrating = function(n, mean, scale.from, scale.to,
                   sd.norm = (scale.to - scale.from + 1) / 5, mean.norm = mean,
                   shift.to.mean = "none") {

  # generate distribution, rounded to units
  dist = round(rnorm(n, mean = mean.norm, sd = sd.norm), digits = 0)

  # calculate breaks for normal distribution
  breaks = c(-Inf, scale.from : (scale.to - 1), +Inf)

  # remove extremes from distribution
  bounded = as.numeric(cut(dist, breaks = breaks, labels = c(scale.from:scale.to)))

  # if no shift to mean selected, return
  if(shift.to.mean == "none")
    return(bounded)

  # sample and shift as many observations as possible at each round
  else if(shift.to.mean == "quick") {

    # calculte how many shifts are needed
    shifts.left = round((mean - mean(bounded)) * n)

    while(shifts.left != 0) {
      # how many observations can be shifted in this round?
      n.shiftable = sum(bounded != ifelse(shifts.left < 0, scale.from, scale.to))
      shifts.this.round = min(abs(shifts.left), n.shiftable)
      # shift
      to.shift = sample(which(bounded != ifelse(shifts.left < 0, scale.from, scale.to)),
                        shifts.this.round)
      bounded[to.shift] = ifelse(shifts.left < 0, bounded[to.shift] - 1, bounded[to.shift] + 1)
      # recalculate n of shifts left
      shifts.left = abs(shifts.left) - shifts.this.round
    }
    return(bounded)
  }

   # sample and shift one observation at each round
  else if(shift.to.mean == "iterative") {
    # calculte how many shifts are needed
    shifts.left = round((mean - mean(bounded)) * n)
    for (i in 1:abs(shifts.left)) {
      to.shift = sample(which(bounded != ifelse(shifts.left < 0, scale.from, scale.to)), 1)
      bounded[to.shift] = ifelse(shifts.left < 0, bounded[to.shift] - 1, bounded[to.shift] + 1)
    }
    return(bounded)
  }
  else
    return(NULL)
}






