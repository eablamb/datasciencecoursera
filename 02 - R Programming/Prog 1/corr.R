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
      res <- c(res, cor(x = data$sulfate, y = data$nitrate, use="na.or.complete", method = "pearson"))
    }
  }
  res
}