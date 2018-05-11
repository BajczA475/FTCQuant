#' Freeze/Thaw cycle parameter sweeping analysis generator
#'
#' This function is designed to automate the process of running the freeze.thaw.sweep function across a series of data sets (e.g., different years worth of data). It also combines the results into a pair of easy to manipulate data frames for further use. See the documentation for freeze.thaw.sweep for more details.
#' @usage freeze_thaw_analysis(data.list, col.start, mag.vec = 0, dur.vec = 1, thres.vec = 0)
#' @param data.list A list, the entries of which are data frames (which need not be named). Each data frame must be structured in a similar manner for the function to operate properly. First, the first column in each must be a column entitled date which contains the date/time information to be passed along to dependent functions. Second, all subsequent columns must contain temperature data, which the function will assume is contemporaneous with those date/time data provided in the date column. It will also assume each column contains temperature data from a single replicable unit.
#' @param mag.vec,dur.vec,thres.vec Vectors of minimum magnitudes, durations, and temperature thresholds to pass along to the freeze.thaw.sweep function. See the documentation of that function for more details. Each defaults to an uninformed value of 0 (mag.vec, thres.vec) or 1 (dur.vec).
#' @keywords freeze thaw cycles parameter sweep temperature time
#' @export
#' @examples
#' freeze.thaw.analysis()

freeze.thaw.analysis = function(data.list, mag.vec = 0, dur.vec = 1, thres.vec = 0) {

Master.data.sheet = numeric(0) #Set up empty storage object for the results.
#Critical for loop for cycling through the three years worth of data sets that we have.
for(sheet in 1:length(data.list)) {

  data = data.list[[sheet]] #Snag out the data set from the list we'll be working with in this year.

#Critical for loop for cycling through the plot data--it runs the parameter sweep we want to run and then stores the results in a Master data sheet.
  for(col in 2:length(data[1,])) {

  array1 = freeze.thaw.sweep(time.vec = data$date, durations=dur.vec, magnitudemins = mag.vec, thresholds = thres.vec, temp.vec = data[,col])

  datasheet1 = reshape2::melt(array1) #Reshape the data into long form.
  freeze_thaw_nos = datasheet1$value #Snag the last column of that reshaped data and make it a vector.
  Master.data.sheet = rbind(Master.data.sheet, freeze_thaw_nos) #Stick the vector in as a row in the Master data sheet.
  print(paste("Array", col, "is done", sep=" "))
  }
  print(paste("Sheet", sheet, "is done", sep=" "))
}

#Now, let's clean up the Master Data Sheet. First, we'll give the columns names...
colnames(Master.data.sheet) = paste("Def", 1:(length(mag.vec)*length(dur.vec)*length(thres.vec)), sep="")
rownames(Master.data.sheet) = NULL #Remove row names.
Master.data.sheet = data.frame(Master.data.sheet) #Make it a data frane.

#Let's also grab a key as to what parameter combinations were ran.
Definition.key = melt(array1, varnames = c("Duration", "Min_Magnitude", "Temp_Threshold"))[,-4]
Definition.key = tibble::add_column(Definition.key, Definition =
                              paste("Def", 1:(length(mag.vec)*length(dur.vec)*length(thres.vec)), sep=""), .before = "Duration")

return(list(data=Master.data.sheet, Definition.key = Definition.key))
}
