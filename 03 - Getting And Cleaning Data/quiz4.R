# Getting and Cleaning Data, Quiz 4 Answers

library(data.table) # for fread()
library(plyr)

setwd('C:/Users/eugene_lamb/Projects/Data Science/Getting And Cleaning Data')

# Question 1
# The American Community Survey distributes downloadable data about United States 
# communities. Download the 2006 microdata survey about housing for the state of 
# Idaho using download.file() from here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# 
# and load the data into R. The code book, describing the variable names is here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# 
# Apply strsplit() to split all the names of the data frame on the characters "wgtp". 
# What is the value of the 123 element of the resulting list?

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
download.file(url = fileUrl, destfile = 'getdata_data_ss06hid.csv', method = 'curl')
dateDownload1 <- date()

idaho2006 <- read.csv('getdata_data_ss06hid.csv')
answer1 <- strsplit(names(idaho2006), 'wgtp')[[123]]

# Question 2
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# 
# Remove the commas from the GDP numbers in millions of dollars and average them. What is the average?
# 
# Original data sources: http://data.worldbank.org/data-catalog/GDP-ranking-table 

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
download.file(url = fileUrl, destfile = 'getdata_data_FGDP.csv', method = 'curl')
dateDownload2 <- date()

fGDP <- read.csv('getdata_data_FGDP.csv')
numsGDP <- as.numeric(gsub(",","",fGDP[5:194,]$X.3))
answer2 <- mean(numsGDP[!is.na(numsGDP)])

# Question 3
# In the data set from Question 2 what is a regular expression that would allow you to count 
# the number of countries whose name begins with "United"? Assume that the variable with the 
# country names in it is named countryNames. How many countries begin with United? 

answer3 <- 'grep("^United",countryNames), 3'

# Question 4
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# 
# Load the educational data from this data set:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
# 
# Match the data based on the country shortcode. Of the countries for which the end of the 
# fiscal year is available, how many end in June?
# 
# Original data sources:
#   http://data.worldbank.org/data-catalog/GDP-ranking-table
# http://data.worldbank.org/data-catalog/ed-stats 

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv '
download.file(url = fileUrl, destfile = 'getdata_data_FEDSTATS_Country.csv', method = 'curl')
dateDownload4 <- date()

fedStatsCountry <- read.csv('getdata_data_FEDSTATS_Country.csv')

labelsGDP <- c('CountryCode','Rank','GDP')
fGDPLabeled <- fGDP[fGDP$X,c(1,2,5)]
names(fGDPLabeled) <- labelsGDP
fGDPLabeled$Rank <- as.integer(as.character(fGDPLabeled$Rank))
rankedGDP <- fGDPLabeled[!is.na(fGDPLabeled$Rank),]
rownames(rankedGDP) <- 1:nrow(rankedGDP)

mergedData <- merge(fedStatsCountry, rankedGDP, by.x=c('CountryCode'), by.y='CountryCode')

answer4 <- length(grep("Fiscal year end: June", mergedData$Special.Notes))

# Question 5
# You can use the quantmod (http://www.quantmod.com/) package to get historical stock prices 
# for publicly traded companies on the NASDAQ and NYSE. Use the following code to download data 
# on Amazon's stock price and get the times the data was sampled.
# 
# library(quantmod)
# amzn = getSymbols("AMZN",auto.assign=FALSE)
# sampleTimes = index(amzn) 
# 
# How many values were collected in 2012? How many values were collected on Mondays in 2012?

library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

df <- as.data.frame(amzn)
dates <- as.data.frame(rownames(df))
names(dates) <- c('Date')
answer5a <- length(dates[format(as.Date(dates$Date),"%Y") == "2012",])
answer5b <- length(dates[format(as.Date(dates$Date),"%Y") == "2012" & weekdays(as.Date(dates$Date)) == "Monday",])
answer5 <- c(answer5a, answer5b)
