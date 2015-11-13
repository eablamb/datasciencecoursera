# Exploratory Data Analysis: Project #1

setwd('C:/Users/eugene_lamb/Projects/Data Science/Exploratory Data Analysis/Project 1')

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

makePlot1 <- function(power) {
    # Plot 1 in PNG form
    
    plot1(power)
    dev.copy(png, 'plot1.png')
    dev.off()
}

makePlot2 <- function (power) {
    # Plot 2 in PNG form
    
    plot2(power)
    dev.copy(png, 'plot2.png')
    dev.off()
}

makePlot3 <- function(power) {
    # Plot 3 in PNG form
    
    png(width = 480, height = 480, file = 'plot3.png')
    plot3(power)
    dev.off()
}


makePlot4 <- function(power) {
    # Plot 4 in PNG form
    
    png(width = 480, height = 480, file = 'plot4.png')
    plot4(power)
    dev.off()
}