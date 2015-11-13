# Getting and Cleaning Data, Quiz 3 Answers

library(jpeg)
library(XML) # for handling XML files
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
# Create a logical vector that identifies the households on greater than 10 acres who 
# sold more than $10,000 worth of agriculture products. Assign that logical vector to 
# the variable agricultureLogical. Apply the which() function like this to identify the 
# rows of the data frame where the logical vector is TRUE. which(agricultureLogical) 
# What are the first 3 values that result?
#
# 153 ,236, 388
# 59, 460, 474
# 125, 238,262
# 403, 756, 798

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
download.file(url = fileUrl, destfile = 'getdata_data_ss06hid.csv', method = 'curl')
dateDownload1 <- date()

idaho2006 <- read.csv('getdata_data_ss06hid.csv')
agriculturalLogical <- with(idaho2006, ACR == 3 & AGS == 6)
answer1 <- head(which(agriculturalLogical), n=3)

# Question 2
# Using the jpeg package read in the following picture of your instructor into R
# 
# https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg
# 
# Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting 
# data? (some Linux systems may produce an answer 638 different for the 30th quantile)
# 10904118 -594524
# -15259150 -10575416
# -15259150 -594524
# -16776430 -15390165

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg'
download.file(url = fileUrl, destfile = 'getdata_jeff.jpg', method = 'curl')
dateDownload2 <- date()

jeff <- readJPEG('getdata_jeff.jpg', native = TRUE) 
answer2 <- quantile(jeff, probs=c(0.3,0.8))

# Question 3
# Load the Gross Domestic Product data for the 190 ranked countries in this data set:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv
# 
# Load the educational data from this data set:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv
# 
# Match the data based on the country shortcode. How many of the IDs match? Sort the data frame in descending order by GDP rank (so United States is last). What is the 13th country in the resulting data frame?
# 
# Original data sources:
#   http://data.worldbank.org/data-catalog/GDP-ranking-table
# http://data.worldbank.org/data-catalog/ed-stats
# 190, Spain
# 234, Spain
# 190, St. Kitts and Nevis
# 234, St. Kitts and Nevis
# 189, Spain
# 189, St. Kitts and Nevis

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
download.file(url = fileUrl, destfile = 'getdata_data_FGDP.csv', method = 'curl')
dateDownload3a <- date()

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv '
download.file(url = fileUrl, destfile = 'getdata_data_FEDSTATS_Country.csv', method = 'curl')
dateDownload3b <- date()

fGDP <- read.csv('getdata_data_FGDP.csv')
fedStatsCountry <- read.csv('getdata_data_FEDSTATS_Country.csv')

# Part 1
labelsGDP <- c('CountryCode','Rank','GDP')
fGDPLabeled <- fGDP[fGDP$X,c(1,2,5)]
names(fGDPLabeled) <- labelsGDP
fGDPLabeled$Rank <- as.integer(as.character(fGDPLabeled$Rank))
rankedGDP <- fGDPLabeled[!is.na(fGDPLabeled$Rank),]
rownames(rankedGDP) <- 1:nrow(rankedGDP)

mergedData <- merge(fedStatsCountry, rankedGDP, by.x=c('CountryCode'), by.y='CountryCode')
answer3a <- as.character(length(mergedData$CountryCode))

# Part 2
mergedData$Rank <- as.integer(as.character(mergedData$Rank))
answer3b <- as.character(arrange(mergedData, desc(Rank))$Table.Name[13])

answer3 <- c(answer3a, answer3b)

# Question 4
# What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
# 32.96667, 91.91304
# 133.72973, 32.96667
# 30, 37
# 23.966667, 30.91304
# 23, 45
# 23, 30

answer4a <- mergedData[mergedData$Income.Group == 'High income: OECD', 'Rank']
answer4a <- as.character(mean(answer4a[!is.na(answer4a)]))
answer4b <- mergedData[mergedData$Income.Group == 'High income: nonOECD', 'Rank']
answer4b <- as.character(mean(answer4b[!is.na(answer4b)]))
answer4 <- c(answer4a, answer4b)

# Question 5
# Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.Group. How many countries 
# are Lower middle income but among the 38 nations with highest GDP?
# 13
# 3
# 5
# 12

rankedData <- mergedData[!is.na(mergedData$Rank),]
answer5 <- nrow(rankedData[rankedData$Rank < 39 & rankedData$Income.Group == 'Lower middle income',])
