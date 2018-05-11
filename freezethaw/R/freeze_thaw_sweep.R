#' Freeze/Thaw cycle parameter sweeping function
#'
#' This function is designed to take two vectors--a vector of continuous temperature readings and a vector of continuous date/time information--and count the number of freeze/thaw cycles that occurred over the time period in question under a range of operational definitions of a freeze/thaw cycle. Specifically, the function allows manipulation of the duration, temp.threshold, and magnitudemin arguments of the freeze.thaw.count function. See the documentation for that function for details.
#' @usage freeze.thaw.sweep(durations = 1, magnitudemins = 0, thresholds = 0, temp.vec, time.vec)
#' @param temp.vec A numeric vector of consecutive temperature readings in which to search for freeze/thaw cycles.
#' @param time.vec A vector of consecutive date/time data corresponding with the temperature data provided to temp.vec. Importantly, the function assumes this data has no missing data internally and that the time steps remain uniform throughout (e.g., the data were recorded every 30 minutes). If either of these assumptions are not satisfied, this function will produce erroneous results.
#' @param thresholds A numeric vector of length 1 or greater corresponding to the maximum temperatures at which the medium in question is to be considered "frozen." Freeze/thaw cycles will be assumed to be fluctuations about these temperatures. Assumes the temperature data are in Celcius and defaults to 0 degrees.
#' @param durations A numeric vector length 1 or greater corresponding to the minimum numbers of time steps the medium in question must be both "frozen" and "thawed" for in order for a freeze/thaw cycle to have occurred. Defaults to 1 time step.
#' @param magnitudemins A numeric vector of length 1 corresponding to the extents to which temperature must fall below (magnitudemin) the temperature threshold set for the medium in question to be considered "frozen" enough for one's purposes. In other words, the function will not begin checking against the duration length until temperatures reach values fall below those specified by this argument, relative to the threshold. Defaults to 0, indicating that any detectable departure below the threshold value is sufficient. Note: Argument values are to be given as absolute values and should thus be positive.
#' @keywords freeze thaw cycles parameter sweep temperature time
#' @export
#' @examples
#' freeze.thaw.sweep()

freeze.thaw.sweep=function(temp.vec, time.vec, durations = 1, magnitudemins = 0, thresholds = 0) {
  #This function will call freeze.thaw.count multiple times on the same data set, varying either the magnitude parameter, the duration parameter, or both that is being used to define a freeze-thaw cycle. It then generates a heat matrix of the results, showing you how the number of F/T cycles observed changes as a function of variation in those two parameters. Right now, only handles variation in duration and min magnitude, but this could be adjusted with some tweaking to handle varying any two parameters. It can't handle more than two because it wouldn't know how to plot these.

  #Empty matrix to store the output data as we go. The last bit ensures that when the array gets graphed later, it's rows and columns and sheets are marked with the values of the parameters that were tried.
  heat.array = array(rep(0,length(durations)*length(magnitudemins)*length(thresholds)),
                     dim=c(length(durations), length(magnitudemins), length(thresholds)),
                     dimnames=list(c(durations), c(magnitudemins), c(thresholds)))

  #Vary over both of the parameters being considered, running freeze.thaw.count each time and storing the result.
  for(vary1 in 1:length(durations)) {
    for(vary2 in 1:length(magnitudemins)) {
      for(vary3 in 1:length(thresholds)) {
        trial = freeze.thaw.count(temp.vec = temp.vec, time.vec = time.vec, duration=durations[vary1],
                                  magnitudemin=magnitudemins[vary2], temp.threshold = thresholds[vary3])

        #Makes sure even trials that don't result in any freeze/thaw cycles plot correctly as numbers.
        if(is.numeric(trial$number.of.cycles)) {
          heat.array[vary1,vary2, vary3] = trial$number.of.cycles
        } else { heat.array[vary1,vary2,vary3] = 0 }
      }
    }
  }
  return(heat.array)
}
