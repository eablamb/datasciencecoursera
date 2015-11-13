# Exploratory Data Analysis: Project #1, part 2
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

plot2 <- function(data, ylab = 'Global Active Power (kilowatts)') {
    # Creates a plot showing Global Active Power rates across
    # the time range of the data subset
    
    plot(x = data$Datetime, 
         y = data$Global_active_power,
         type = 'l',
         xlab = '',
         ylab = ylab,
         mgp = c(3,1,0))
}

makePlot2 <- function () {
    # Plot 2 in PNG form
    
    power <- getPowerData()
    plot2(power)
    dev.copy(png, 'plot2.png')
    dev.off()
}

makePlot2()