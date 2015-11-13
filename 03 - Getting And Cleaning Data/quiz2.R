# Getting and Cleaning Data, Quiz 2 Answers

library(httr) # for OAuth functions
library(httpuv) # for API functions
library(jsonlite)

setwd('C:/Users/eugene_lamb/Projects/Data Science/Getting And Cleaning Data')

# Question 1
# Register an application with the Github API here https://github.com/settings/applications. 
# Access the API to get information on your instructors repositories (hint: this is the url 
# you want "https://api.github.com/users/jtleek/repos"). Use this data to find the time that 
# the datasharing repo was created. What time was it created? This tutorial may be useful 
# (https://github.com/hadley/httr/blob/master/demo/oauth2-github.r). You may also need to run 
# the code in the base R package and not R studio.
#
# Client ID
# b67053f4f9b695013bc2
# Client Secret
# 66e1be7476cd62e6669a647bfb380173cad909bb

# Find OAuth settings
oauth_endpoints("github")

# After registering my application, retrieve it
myapp <- oauth_app('Le App', key='b67053f4f9b695013bc2', secret='66e1be7476cd62e6669a647bfb380173cad909bb')

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)
siteContent <- content(req)

# Use jsonlite to convert to a data frame
json <- jsonlite::fromJSON(toJSON(siteContent))

answer1 <- subset(json, name == 'datasharing')$created_at


# Question 2
# The sqldf package allows for execution of SQL commands on R data frames. We will use the sqldf package to practice the queries we might send with the dbSendQuery command in RMySQL. Download the American Community Survey data and load it into an R object called
# 
# acs
# 
# 
# 
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
# 
# Which of the following commands will select only the data for the probability weights pwgtp1 with ages less than 50?
# sqldf("select pwgtp1 from acs where AGEP $$\lt$$ 50")
# sqldf("select * from acs where AGEP $$\lt$$ 50")
# sqldf("select pwgtp1 from acs")
# sqldf("select * from acs")


answer2 <- 'sqldf("select pwgtp1 from acs where AGEP $$\lt$$ 50")'


# Question 3
# Using the same data frame you created in the previous problem, what is the equivalent function to unique(acs$AGEP)
# sqldf("select distinct pwgtp1 from acs")
# sqldf("select unique * from acs")
# sqldf("select distinct AGEP from acs")
# sqldf("select unique AGEP from acs")


answer3 <- 'sqldf("select distinct AGEP from acs")'


# Question 4
# How many characters are in the 10th, 20th, 30th and 100th lines of HTML from this page:
#   
#   http://biostat.jhsph.edu/~jleek/contact.html
# 
# (Hint: the nchar() function in R may be helpful)
# 43 99 8 6
# 45 31 7 31
# 43 99 7 25
# 45 31 2 25
# 45 92 7 2
# 45 0 2 2
# 45 31 7 25

con <- url('http://biostat.jhsph.edu/~jleek/contact.html')
htmlCode <- readLines(con)
close(con)

answer4 <- Map(function(n) {nchar(htmlCode[[n]])}, c(10,20,30,100))


# Question 5
# Read this data set into R and report the sum of the numbers in the fourth column.
# 
# https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for
# 
# Original source of the data: http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for
# 
# (Hint this is a fixed width file format)
# 101.83
# 36.5
# 222243.1
# 35824.9
# 28893.3
# 32426.7

fileUrl <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for'
download.file(url = fileUrl, destfile = 'wksst8110.for', method='curl')
dateDownload5 <- date()

sstData <- read.fwf('wksst8110.for', widths=c(9,-6,4,4,-5,4,4,-5,4,4,-5,4,4), skip=4)
answer5 <- sum(sstData$V4)
