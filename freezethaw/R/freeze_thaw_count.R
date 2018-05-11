#' Freeze/Thaw cycle counting function
#'
#' This function is designed to take two vectors--a vector of continuous temperature readings and a vector of continuous date/time information--and count the number of freeze/thaw cycles that occurred over the time period in question. Purposefully, the function is designed to allow for adjustment of the parameters that may define a freeze/thaw cycle. temp.vec and time.vec should usually be from the same data source, but the function will run properly so long as they are equal in length.
#' @usage freeze.thaw.count(temp.vec, time.vec, temp.threshold = 0, duration = 1, magnitudemin = 0, magnitudemax = 0)
#' @param temp.vec A numeric vector of consecutive temperature readings in which to search for freeze/thaw cycles.
#' @param time.vec A vector of consecutive date/time data corresponding with the temperature data provided to temp.vec. Importantly, the function assumes this data has no missing data internally and that the time steps remain uniform throughout (e.g., the data were recorded every 30 minutes). If either of these assumptions are not satisfied, this function will produce erroneous results.
#' @param temp.threshold A numeric value of length 1 corresponding to the maximum temperature at which the medium in question is considered "frozen." Freeze/thaw cycles will be assumed to be fluctuations about this temperature. Assumes the temperature data are in Celcius and thus defaults to 0 degrees.
#' @param duration A numeric value of length 1 corresponding to the minimum number of time steps the medium in question must be both "frozen" and "thawed" for in order for a freeze/thaw cycle to have occurred. Defaults to 1 time step.
#' @param magnitudemin,magnitudemax Numeric values each of length 1 corresponding to the extent to which temperature must fall below (magnitudemin) and/or rise above (magnitude max) the temperature threshold set for the medium in question to be considered "thawed" or "frozen" enough for one's purposes. In other words, the function will not begin checking against the duration length until temperatures reach values beyond those specified by the magnitudemin and magnitudemax arguments. Both default to 0, indicating that any departure above or below the threshold value is sufficient. Note: Both arguments are to be given as absolute values and should be positive.
#' @keywords freeze thaw cycles temperature time
#' @export
#' @examples
#' freeze.thaw.count()


freeze.thaw.count = function(temp.vec, time.vec, temp.threshold = 0, duration = 1, magnitudemin = 0, magnitudemax = 0) {
  ##This function ccounts the number of freeze-thaw cycles that occurred in a data set by checking for consecutive temperature patterns in a consecutive data set. The consecutive word is key because the function assumes that the data presented are in consecutive chronological order by row with no missing time points. The function takes 6 arguments: temp.vec is a vector of consecutive temperature data (presumably a column from a data set); time.vec is the corresponding time data (used primarily for contextualizing output); temp.threshold is the threshold temperature at which the phase change between frozen/thawed occurs (assumed by default to be 0 degrees C); duration is the length of time (in half hours) that the soil needs to be frozen AND thawed for for a freeze-thaw cycle to be recorded; and magnitudemax/magnitudemin are the temperature differences above and below the threshold temperature that the temperature must rise/fall by during the thaw/freeze portions of the cycle for the duration counter to start and keep counting. By default, these are both assumed to be 0 degrees--i.e., the soil is considered thawed/frozen if any deviation from the threshold temperature is acheived.

  #Counters/storage vectors--These are used to track progress and results as they occur.
  cycle.count = 0
  start.vec = numeric(0)
  end.vec = numeric(0)
  freeze.length = numeric(0)
  thaw.length = numeric(0)
  switch.after = numeric(0)
  min.freeze.temp = numeric(0)

  #To simplify the operations of the function, it creates a new vector called new.vec that recodes the temp.vec vector to 0s, 1s, and -1s for rows that are at/within, above, and below the threshold temperatures (with the magnitude factors built in.)
  new.vec = temp.vec

  new.vec[which(temp.vec >= temp.threshold + magnitudemax)] = 1
  new.vec[which(temp.vec <= temp.threshold - magnitudemin)] = -1
  new.vec[which(temp.vec > temp.threshold - magnitudemin & temp.vec < temp.threshold + magnitudemax)] = 0

  #To simplify the operations even further still, the function uses the rle function (run length encoding) to figure out how consecutive runs of each values are contained within the new.vec vector. It then unpacks the runs and their lengths for the rest of the function to work with.
  rle1 = rle(new.vec)
  lengths = rle1$lengths; values = rle1$values

  #Now, the function calculates how many freeze and thaw periods were of sufficient length to be defines as "sufficiently long" to be potentially a part of a freeze thaw cycle. It marks each of these as potential "events" that were constituent to a freeze-thaw cycle.
  long.freezes = which(lengths >= duration & values == -1)
  long.thaws = which(lengths >= duration & values == 1)
  events = sort(c(long.freezes, long.thaws))

  #The important vector is one more storage vector--it is used to keep track of which "events" are actually a part of freeze-thaw cycles.
  important = rep(NA, length(events))

  ##The function now begins in earnest. First, it confirms that there was at least one long freeze and one long thaw during the data in question (and thus there was at least, by definition, one freeze-thaw cycle contained within the data). Then...
  if (length(long.freezes) > 1 & length(long.thaws) > 1) {

    #The function then checks to see if the first "event" of consequence is a freeze event or a thaw event and marks the status accordingly.
    if (values[events[1]] == -1) { status = 1 ; important[1] = 1 } else { status = 0 }

    #Then, for each subsequent event, we see if we are changing from frozen to thawed or vice versa. If we do, we mark the event as important. You'll notice that there are actually three statuses: 0, 1, and 2. 0 and 2 are both thaws, but 0 marks the special case that the ground starts out thawed (which is basically always the case), so we needed 0 to differentiate between that kind of thaw and a mid-season thaw.
    for (event in 2:length(events)) {
      if(values[events[event]] == -1) {
        if (status == 0 ) {
          important[event] = 1
          status = 1
        } else {
          if (status == 2) {
            important[event] = 1
            status = 1
          }
        }

        #If we're thawed and we move into a freeze, this checks for that and updates the important and status objects accordingly.
      } else {
        if (values[events[event]] == 1) {
          if (status == 1) {
            important[event] = 2
            status = 2
          }
        }
      }
    }

    #The number of freeze-thaw cycles is equal to the number of events marked as important with a value of 1 (thaws are marked by 2, so really this is a count of the number of freezes, since every freeze is gauranteed to be part of a F/T cycle at some point)
    cycle.count = length(which(important==1))

    #These next chunks of code generate output measures of importance.
    #For example, this one figures out when each F/T cycle starts by figuring out which events were marked as important with a 1. This figures out exactly what time points in the time.vec mark the start of the F/T cycles found (the beginning of the freeze, specifically).
    starts = events[which(important == 1)]
    start.times = numeric((0))
    for(i in starts) { start.times = c(start.times, (sum(lengths[1:i-1])+1)) }
    start.vec = as.character(time.vec[start.times])

    #Switches are points where the freeze ends and the thaw period begins.
    switches = events[which(important == 2)]
    switch.times = numeric((0))
    for(i in switches) { switch.times = c(switch.times, (sum(lengths[1:i-1]))) }
    switch.after = as.character(time.vec[switch.times])
    if(length(start.vec) != length(switch.after)) { #If the season ends on a freeze with no subsequent thaw, we need to mark
      #this last switch point as "Unknown."
      switch.after = c(switch.after, "Unknown")
    }

    #End points are when the thaw period ends and the next freeze begins (if the season is over and thus the next freeze is not known, the last F/T's endpoint is marked as unknown).
    ends = events[which(important == 1)][-1]
    end.times = numeric((0))
    for(i in ends) { end.times = c(end.times, sum(lengths[1:(i-1)])) }
    end.vec = as.character(time.vec[end.times])
    if(length(start.vec) != length(end.vec)) { #If the season ends on a freeze with no subsequent thaw, we need to mark
      #this last end point as "Unknown."
      end.vec = c(end.vec, "Unknown")
    }

    #To make the next block of code work, need to check to see again if the season ends on a freeze instead of a thaw and
    #account for this.

    if(length(start.times) != length(switch.times)) { switch.times = c(switch.times, NA)}
    if(length(start.times) != length(end.times)) { end.times = c(end.times, NA)}

    #These two calculations figure out how long each freeze and thaw phase is in each cycle.
    freeze.length = (switch.times+1) - start.times
    thaw.length = end.times - switch.times

    #This calculates what the min temperature observed during each freeze period was.
    min.freeze.temp = numeric(0)
    for (freeze in 1:length(start.times)) {
      start.pt = start.times[freeze]
      end.pt = switch.times[freeze]
      if(is.na(end.pt)) { min.freeze.temp = c(min.freeze.temp, NA) } else {
      min.freeze.temp = c(min.freeze.temp, min(temp.vec[start.pt:end.pt], na.rm=T)) }
    }

  } else {cycle.count = "There are no freeze/thaw cycles in this data set, as defined"} #Only occurs if there were no freezes.

  return(list(temp.threshold = temp.threshold, hemicycle.duration.in.hours = duration/2, number.of.cycles = cycle.count,
              cycles = data.frame(start.times = start.vec, switch.after = switch.after, end.times = end.vec, freeze.length = freeze.length/2, thaw.length=thaw.length/2, min.freeze.temp = min.freeze.temp)))
}
