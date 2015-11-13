# Exploratory Data Analysis: Project #1, part 1
# by Geno Lamb

getPowerData <- function() {
    # Downloads and extracts household power consumption data from
    # the Individual household electric power consumption Data Set
    # availabe at the UC Irvine Machine Learning Repository.
    
    # The extracted data is a subset from 2007-02-01 to 2007-02-02
    
    require(utils)
    
    makeDateTime <- function(date, time) {
        d <- as.character(date)
        t <- as.character(time)
        as.POSIXct(strptime(paste(d,t), '%Y-%m-%d %H:%M:%S'))
    }
    
    fileURL <- 'http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    destFile <- paste(getwd(),'exdata_data_household_power_consumption.zip',sep='/')
    print (destFile)
    download.file(url = fileURL, 
                  destfile = destFile)
    unzip(destFile)
    data <- read.table(file = 'household_power_consumption.txt', 
                       header = TRUE, 
                       sep = ';', 
                       na.strings = '?')
    data$Date <- as.Date(data$Date, "%d/%m/%Y")
    power <- subset(data, Date == as.Date('2007-02-01') | 
                        Date == as.Date('2007-02-02'))
    power$Datetime <- as.POSIXct(mapply(makeDateTime, power$Date, power$Time),
                                 origin = '1970-01-01')
    power
}

plot1 <- function(data) {
    # Creates a frequency histogram of the number of occurences
    # of distinct Global Active Power values (in kilowatts)
    
    hist(data$Global_active_power, 
         freq = TRUE,
         col = 'red', 
         main = 'Global Active Power',
         xlab = 'Global Active Power (kilowatts)',
         ylim = c(0,1200))
}

makePlot1 <- function() {
    # Plot 1 in PNG form
    
    power <- getPowerData()
    plot1(power)
    dev.copy(png, 'plot1.png')
    dev.off()
}

makePlot1()
