# R Programming - Programming Assignment #1

setwd('C:/Users/eugene_lamb/Projects/Data Science/R Programming/Prog 1/')

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
     count = count + nrow(pollutantData)
     sum = sum + sum(pollutantData)
  }
  result = round(sum / count, 3)
  result
}


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
  nobs <- subset(nobs, nobs != 'NA')
  
  res <- data.frame(cbind(id,nobs))
  res <- res[order(res$nobs, decreasing=TRUE),]
  res
}


corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  res <- c()
  for (f in list.files(directory)) {
    fpath = paste(directory, f, sep='/')
    data <- read.csv(fpath)
    numComplete <- sum(complete.cases(data))
    if (numComplete > threshold) {
      res <- c(res, round(cor(x = data$sulfate, y = data$nitrate, use="na.or.complete", method = "pearson"), 5))
    }
  }
  res
}