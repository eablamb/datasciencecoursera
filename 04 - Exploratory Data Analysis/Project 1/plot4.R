# Exploratory Data Analysis: Project #1, part 4
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

plot4 <- function(data) {
    # Creates 4 plots (clockwise from top left)
    # Plot 1 - Global Active Power consumption over the subset data time range
    # Plot 2 - minute-averaged Voltage over the subset data time range
    # Plot 3 - Energy sub metering measurements over the subset data time range
    # Plot 4 - Global reactive power consumption over the subset data time range
    
    plotVoltage <- function(data) {
        plot(x = data$Datetime,
             y = data$Voltage,
             type = 'l',
             xlab = 'datetime',
             ylab = 'Voltage')
    }
    
    plotGlobalReactivePower <- function(data) {
        plot(x = data$Datetime,
             y = data$Global_reactive_power,
             type = 'h',
             xlab = 'datetime',
             ylab = 'Global_reactive_power')
    }
    
    par(mfrow = c(2,2))
    with(data, {
        plot2(data, ylab = 'Global Active Power')
        plotVoltage(data)
        plot3(data, bty = 'n')
        plotGlobalReactivePower(data)
    })
}

makePlot4 <- function() {
    # Plot 4 in PNG form
    
    power <- getPowerData()
    png(width = 480, height = 480, file = 'plot4.png')
    plot4(power)
    dev.off()
}

makePlot4()
