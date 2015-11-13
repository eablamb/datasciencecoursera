complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  nobs = c()
  for (i in id) {
    fpath = paste(directory,sprintf("%03d.csv",i),sep='/')
    data <- read.csv(fpath)
    nobs[[i]] <- sum(complete.cases(data))
  }
  
  res <- data.frame(cbind(id,'nobs'=nobs[id]))
  resNoNA <- subset(res, id != 'NA' & nobs != 'NA')
  resNoNA
}