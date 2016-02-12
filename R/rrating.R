#' Simulate Likert-like Item Responses
#'
#' Generates random distributions of answers to Likert-type items
#' answered on a numeric (integer) scale.
#'
#'
#' @param n number of observations.
#' @param mean mean of the normal distribution to be approximated.
#' @param scale.from bottom-end of the possible values in scale.
#' @param scale.to top-end of the possible values in scale.
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
rrating = function(n, mean = 3, scale.from = 1, scale.to = 5,
                   sd.norm = 1, shift.to.mean = "none") {

  # TODO: default SD according to scale range
  sd = sd.norm

  # generate distribution, rounded to units
  dist = round(rnorm(n, mean = mean, sd = sd), digits = 0)

  # calculate breaks for normal distribution
  breaks = c(-Inf, scale.from : (scale.to - 1), +Inf)

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






