# Getting and Cleaning Data, Quiz 1 Answers

library(xlsx) # for handling Excel files
library(XML) # for handling XML files
library(data.table) # for fread()

setwd('C:/Users/eugene_lamb/Projects/Data Science/Getting And Cleaning Data')

# Question 1
# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() 
# from here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# 
# and load the data into R. The code book, describing the variable names is here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# 
# How many properties are worth $1,000,000 or more?

fileUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
download.file(url = fileUrl, destfile = 'getdata_data_ss06hid.csv', method = 'auto')
dateDownload1 <- date()

housingSurveyIdaho2006 <- read.csv('getdata_data_ss06hid.csv')
answer1 <- nrow(subset(housingSurveyIdaho2006, VAL == 24, select=VAL))


# Question 2
# Use the data you loaded from Question 1. Consider the variable FES in the code book. 
# Which of the "tidy data" principles does this variable violate?

answer2 <- 'Tidy data has one variable per column'


# Question 3
# Download the Excel spreadsheet on Natural Gas Aquisition Program here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx
# 
# Read rows 18-23 and columns 7-15 into R and assign the result to a variable called:
#   
#   dat 
# 
# What is the value of:
#   
#   sum(dat$Zip*dat$Ext,na.rm=T) 
# 
# (original data source: http://catalog.data.gov/dataset/natural-gas-acquisition-program)

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx'
download.file(url = fileUrl, destfile = 'NGAP.xlsx', method='curl')
dateDownload3 <- date()

dat <- read.xlsx('NGAP.xlsx', sheetIndex=1, colIndex=7:15, rowIndex=17:23)
answer3 <- sum(dat$Zip*dat$Ext,na.rm=T)

# Question 4
# Read the XML data on Baltimore restaurants from here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
# 
# How many restaurants have zipcode 21231?

fileUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml'
download.file(url = fileUrl, destfile = 'getdata_data_restaurants.xml', method = 'auto')
dateDownload4 <- date()

baltimoreRestaurantsData <- xmlTreeParse('getdata_data_restaurants.xml', useInternal=TRUE)
rootNode <- xmlRoot(baltimoreRestaurantsData)
zipcodes <- xpathSApply(rootNode, '//zipcode', xmlValue)
answer4 <- length(zipcodes[zipcodes == '21231'])


# Question 5
# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() 
# from here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
# 
# using the fread() command load the data into an R object
# 
# DT 
# 
# Which of the following is the fastest way to calculate the average value of the variable
# 
# pwgtp15 
# 
# broken down by sex using the data.table package? 

fun_strs <- c(
#  'rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]',  	
  'sapply(split(DT$pwgtp15,DT$SEX),mean)',	
  'mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)',		
  'DT[,mean(pwgtp15),by=SEX]',			
  'tapply(DT$pwgtp15,DT$SEX,mean)',			
  'mean(DT$pwgtp15,by=DT$SEX)'
)

funs <- list(
#  (function(DT) {rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]}),		
  (function(DT) {sapply(split(DT$pwgtp15,DT$SEX),mean)}),	
  (function(DT) {mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)}),		
  (function(DT) {DT[,mean(pwgtp15),by=SEX]}),			
  (function(DT) {tapply(DT$pwgtp15,DT$SEX,mean)}),			
  (function(DT) {mean(DT$pwgtp15,by=DT$SEX)})
)
times <- list()

fileUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv'
download.file(url = fileUrl, destfile = 'getdata_data_ss06pid.csv', method = 'auto')
dateDownload5 <- date()

DT <- fread('getdata_data_ss06pid.csv')
for (i in 1:length(funs)) {
  times[[i]] <- mean(microbenchmark(funs[[i]](DT))[[2]])
}
names(times) <- fun_strs
answer5 <- names(which.min(times))
