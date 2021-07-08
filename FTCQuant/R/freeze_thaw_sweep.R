#' Freeze-thaw cycle counting function that sweeps over a set of defining criteria
#'
#' This function is designed to take two vectors--a vector of continuous temperature readings and a vector of continuous date-time data--and count the number of freeze-thaw cycles (FTCs) that occurred over the time period in question under a range of possible criteria. Specifically, the function allows manipulation of the duration, temp.threshold, and lower.offset arguments of the freeze.thaw.count function. See the documentation for that function for details on its operations.
#' @usage freeze.thaw.sweep(temp.vec, time.vec, durations = 1, lower.offsets = 0, thresholds = 0)
#' @param temp.vec A numeric vector of consecutive temperature readings in which to search for freeze-thaw cycles. These are assumed to be fully continuous and not missing any data not marked by NAs, as described in freeze.thaw.count.
#' @param time.vec A vector of consecutive date-time data corresponding with the temperature data provided to temp.vec. Importantly, the function assumes these data have no missing data and that the time steps remain uniform throughout. If either of these assumptions are not satisfied, this function will produce erroneous results!
#' @param thresholds A numeric vector of length 1+ corresponding to the maximum temperatures at which the medium in question is to be considered "frozen." Freeze-thaw cycles will be assumed to be fluctuations about these temperatures at a minimum. Assumes the temperature data are in Celcius and thus defaults to 0 degrees but the function can operate on any temperature scale.
#' @param durations A numeric vector length 1+ corresponding to the minimum numbers of time steps the medium in question must be both "frozen" and "thawed" in order for a freeze-thaw cycle to have occurred. Defaults to 1 time step.
#' @param lower.offsets A numeric vector of length 1 or more corresponding to the extents to which temperature must fall below  the temperature threshold set for the medium in question to be considered "effectively frozen." In other words, the function will not begin checking against the duration length until temperatures reach values  below those specified by this argument, relative to the temp.threshold. Defaults to 0, indicating that any detectable drop below the threshold value is sufficient. Note: Values for this argument are to be given as absolute values and should thus always be positive.
#' @keywords freeze thaw cycles parameter sweep temperature time
#' @export
#' @examples
#' freeze.thaw.sweep()

freeze.thaw.sweep=function(temp.vec, time.vec, durations = 1, lower.offsets = 0, thresholds = 0) {
  #This function will call freeze.thaw.count multiple times on the same data set. Each time, it will vary either the duration, magnitude.min, or temperature threshold parameter until all possible combinations given are exhausted. It then generates an array showing how many freeze-thaw cycles (FTCs) were counted under each parameter combination. Right now, it only handles variation in the three parameters mentioned above, but it could be amended to allow additional parameters to be swept.

  #We start by creating an empty matrix to store the output data. The last bit ensures that when the array gets graphed, its rows, columns, and sheets are marked appropriately with the parameter values from the parameter sweep.
  heat.array = array(rep(0,length(durations)*length(lower.offsets)*length(thresholds)),
                     dim=c(length(durations), length(lower.offsets), length(thresholds)),
                     dimnames=list(c(durations), c(lower.offsets), c(thresholds)))

  #Next, the function conducts a series of calls to freeze.thaw.count, making one variation in parameter values per new call until all combinations have been exhausted. The results (number of FTCs counted) are stored in our heat.array each call.
  for(vary1 in 1:length(durations)) {
    for(vary2 in 1:length(lower.offsets)) {
      for(vary3 in 1:length(thresholds)) {
        trial = freeze.thaw.count(temp.vec = temp.vec, time.vec = time.vec, duration=durations[vary1],
                                  lower.offset=lower.offsets[vary2], temp.threshold = thresholds[vary3])

        #This makes sure trials that don't result in any FTCs plot correctly as numbers if that is later intended.
        if(is.numeric(trial$number.of.cycles)) {
          heat.array[vary1,vary2, vary3] = trial$number.of.cycles
        } else { heat.array[vary1,vary2,vary3] = 0 }
      }
    }
  }
  return(heat.array)
}
