# Exploratory Data Analysis: Project #1, part 3
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

plot3 <- function(data, bty = 'o') {
    # Creates a plot showing Energy sub-metering measurements
    # across the time range of the data subset
    
    with(data, {
        plot(x = Datetime, 
             y = Sub_metering_1, 
             type = 'l', 
             col = 'black', 
             xlab = '',
             ylab = 'Energy sub metering')
        lines(x = Datetime, y = Sub_metering_2, col = 'red')
        lines(x = Datetime, y = Sub_metering_3, col = 'blue')
        legend('topright', 
               col = c('black', 'red', 'blue'), 
               legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
               lty = c(1,1,1),
               bty = bty)
    })
}

makePlot3 <- function() {
    # Plot 3 in PNG form
    
    power <- getPowerData()
    png(width = 480, height = 480, file = 'plot3.png')
    plot3(power)
    dev.off()
}

makePlot3()