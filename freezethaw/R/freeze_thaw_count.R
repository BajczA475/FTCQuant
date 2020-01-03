#' Basic freeze-thaw cycle (FTC) counting function
#'
#' This function is designed to take two vectors--a vector of continuous temperature readings and a vector of continuous date-time data--and count the number of freeze-thaw cycles (FTCs) that occurred over the given time period. Purposefully, the function is designed to allow for adjustment of the criteria that define a freeze-thaw cycle. temp.vec and time.vec should usually be from the same data source, but the function will run so long as they are equal in length.
#' @usage freeze.thaw.count(temp.vec, time.vec, temp.threshold = 0, duration = 1, magnitudemin = 0, magnitudemax = 0)
#' @param temp.vec A numeric vector of consecutive temperature readings in which to search for freeze-thaw cycles. It is assumed that the data are continuous and missing no data. If either assumption is violated, the output data will be inaccurate!
#' @param time.vec A vector of consecutive data-time data corresponding with the temperature data provided to temp.vec. Importantly, the function assumes this data has no missing data internally and that the time steps remain uniform throughout (e.g., the data were recorded every 30 minutes without fail). If either of these assumptions are not satisfied, this function will produce erroneous results, as noted above.
#' @param temp.threshold A numeric value of length 1 corresponding to the maximum temperature at which the medium in question is considered "frozen." Freeze-thaw cycles will be assumed to be fluctuations about this temperature at a minimum. Defaults to 0  under the assumption the data given to temp.vec are in degrees Celcius but can be switched to any value to accomodate other temperature scales.
#' @param duration A numeric value of length 1 corresponding to the minimum number of time steps the medium in question must be both "frozen" and "thawed" for a complete freeze-thaw cycle to have occurred. Defaults to 1, as in one time step.
#' @param magnitudemin,magnitudemax Numeric values each of length 1 corresponding to the extent to which the temperature must fall below (magnitudemin) and/or rise above (magnitudemax) the temperature threshold set for the medium in question to be considered "effectively thawed" or "effective frozen." In other words, the function will not begin checking against the duration length until temperatures reach values beyond those specified by the magnitudemin and magnitudemax arguments (in combination with the threshold argument). Both default to 0, indicating that any departure above or below the threshold value is sufficient. Note: Both arguments are to be given as absolute values (deviations from the temperature threshold) and thus should be specificed as positive values.
#' @keywords freeze thaw cycles temperature time
#' @export
#' @examples
#' freeze.thaw.count()


freeze.thaw.count = function(temp.vec, time.vec, temp.threshold = 0, duration = 1, magnitudemin = 0, magnitudemax = 0) {
  ##This function counts the number of freeze-thaw cycles that occurred in a data set by checking for specific patterns in a consecutive temperature data set. The consecutiveness is key because the function assumes that the data presented are in consecutive chronological order by row with no missing time points. The function takes 6 arguments: temp.vec is a vector of consecutive temperature data (presumably a column from a data set); time.vec is the corresponding time data (used primarily for contextualizing output); temp.threshold is the threshold temperature at which the phase change between frozen/thawed occurs (assumed by default to be 0 degrees C); duration is the length of time (whatever the units of the time.vec are) that the soil needs to be frozen AND thawed for for a complete freeze-thaw cycle to be recorded; and magnitudemax/magnitudemin are the temperature differences above and below the threshold temperature that the temperature must rise/fall by during the thaw/freeze portions of the cycle for the duration counter to start counting. By default, these are both assumed to be 0 degrees--i.e., the soil is considered thawed/frozen if any deviation from the threshold temperature occurs.

  #Counters/storage vectors--These are used to track progress and results as they occur. Modifications here could allow for additional output variables to be reported, if desired.
  cycle.count = 0
  start.vec = numeric(0)
  end.vec = numeric(0)
  freeze.length = numeric(0)
  thaw.length = numeric(0)
  switch.after = numeric(0)
  min.freeze.temp = numeric(0)

  #To greatly simplify the operations of the function, it creates a new vector called new.vec that recodes the temp.vec vector. It converts the temp.vec to only 0s, 1s, and -1s for rows that are transitional, effectively thawed, and effectively frozen, respectively, with these three regions defined by the temp.threshold, magnitudemin, and magnitudemax arguments.
  new.vec = temp.vec

  new.vec[which(temp.vec >= temp.threshold + magnitudemax)] = 1
  new.vec[which(temp.vec <= temp.threshold - magnitudemin)] = -1
  new.vec[which(temp.vec > temp.threshold - magnitudemin & temp.vec < temp.threshold + magnitudemax)] = 0

  #To simplify the operations even further, the function uses the rle function (run length encoding) to figure out how consecutive runs of each values are contained within the new.vec vector. It then unpacks the runs and their lengths for the rest of the function to work with.
  rle1 = rle(new.vec)
  lengths = rle1$lengths; values = rle1$values

  #Now, the function calculates how many effectively frozen and effectively thawed periods were of sufficient length to be long enough to be count as part of a freeze-thaw cycle given the criteria specified in the function call. It marks each of these as potential "events" that were constituent to a freeze-thaw cycle (internal function logic).
  long.freezes = which(lengths >= duration & values == -1)
  long.thaws = which(lengths >= duration & values == 1)
  events = sort(c(long.freezes, long.thaws))

  #The important vector is another storage vector--it is used to keep track of which "events" are actually a part of a freeze-thaw cycle and which are not.
  important = rep(NA, length(events))

  ##The function operations now begin. First, the function confirms that there was at least one sufficiently long freeze period and one sufficiently long thaw in the data in question (and thus there was at least, by definition, one freeze-thaw cycle contained within the data).
  if (length(long.freezes) > 1 & length(long.thaws) > 1) {

    #Assuming there is one, the function then checks to see if the first "event" of consequence is a freeze event or a thaw event and marks the status accordingly. The status scalar is described below.
    if (values[events[1]] == -1) { status = 1 ; important[1] = 1 } else { status = 0 }

    #Then, for each subsequent event, the function checks to see if we are changing from frozen to thawed or vice versa. If we change statuses, we mark the event as important in the important vector. You'll notice that there are actually three statuses: 0, 1, and 2. 0 and 2 are both thaws, but 0 marks the special case that the ground starts out thawed, so we needed a second code for thawed to differentiate between that kind of thaw and a mid-season thaw.
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

    #The number of freeze-thaw cycles is equal to the number of events marked as important with a value of 1 (thaws are marked by a value of 2 instead, so really this is a count of the number of freezes, since every freeze is gauranteed to be part of a F/T cycle at some point, although it should be noted that this may not always be the intention of the end user)
    cycle.count = length(which(important==1))

    #These next chunks of code generate output measures we thought might be of importance. It's not intended to be an exhaustive list and others should feel free to add to it.
    #For example, this code chunk figures out when each FTC starts by figuring out which events were marked as important with a 1. This figures out exactly what time points in the time.vec mark the start of the FTC cycles found (the exact beginning of the freeze, specifically).
    starts = events[which(important == 1)]
    start.times = numeric((0))
    for(i in starts) { start.times = c(start.times, (sum(lengths[1:i-1])+1)) }
    start.vec = as.character(time.vec[start.times])

    #Switches are points where the freeze ends and the thaw period begins. These are determined in much the same way.
    switches = events[which(important == 2)]
    switch.times = numeric((0))
    for(i in switches) { switch.times = c(switch.times, (sum(lengths[1:i-1]))) }
    switch.after = as.character(time.vec[switch.times])
    if(length(start.vec) != length(switch.after)) { #If the season ends on a freeze with no subsequent thaw, we need to mark
      #this last switch point as "Unknown."
      switch.after = c(switch.after, "Unknown")
    }

    #End points are when the thaw period ends and the next freeze begins (if the season is over and thus the next freeze is not known, the last FTC endpoint is marked as unknown similar to above).
    ends = events[which(important == 1)][-1]
    end.times = numeric((0))
    for(i in ends) { end.times = c(end.times, sum(lengths[1:(i-1)])) }
    end.vec = as.character(time.vec[end.times])
    if(length(start.vec) != length(end.vec)) { #If the season ends on a freeze with no subsequent thaw, we need to mark
      #this last end point as "Unknown."
      end.vec = c(end.vec, "Unknown")
    }

    #To make the next block of code work, the function first needs to check to see if the season ends on a freeze instead of a thaw and account for this by reporting NA in the switch.times and end.times output vectors.

    if(length(start.times) != length(switch.times)) { switch.times = c(switch.times, NA)}
    if(length(start.times) != length(end.times)) { end.times = c(end.times, NA)}

    #These two calculations figure out how long each freeze and thaw phase was in each FTC.
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

    #If there were no freeze periods of sufficient length in the data set, this is reported as output and the function ends.
  } else {cycle.count = "There are no freeze-thaw cycles in this data set, as these were defined by the input criteria"}

  return(list(temp.threshold = temp.threshold, hemicycle.duration.in.hours = duration/2, number.of.cycles = cycle.count,
              cycles = data.frame(start.times = start.vec, switch.after = switch.after, end.times = end.vec, freeze.length = freeze.length/2, thaw.length=thaw.length/2, min.freeze.temp = min.freeze.temp)))
}
