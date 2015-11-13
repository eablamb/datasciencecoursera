pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  sum = 0
  count = 0
  result = 0
  
  for (i in id) {
    fpath = paste(directory,sprintf("%03d.csv",i),sep='/')
    data <- read.csv(fpath)
    pollutantData <- subset(data[pollutant], data[pollutant]!='NA')
    numComplete <- nrow(pollutantData)
    if (numComplete > 0) {
      count = count + numComplete
      sum = sum + sum(pollutantData)
    }
  }
  result = round(sum / count, 3)
  result
}