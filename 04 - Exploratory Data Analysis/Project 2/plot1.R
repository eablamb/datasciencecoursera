getPMData <- function(plotFunc) {
    # Downloads and extracts fine particulate matter data from
    # the National Emmissions Invetory (NEI) Data Set
    # availabe at the EPA National Emissions Invetory web site.
    
    # The extracted data is a subset from 1999, 2002, 2005, and 2008
    
    require(utils)
    require(RDS)
    
    fileURL <- 'http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
    destFile <- paste(getwd(),'exdata_data_FNEI_data.zip',sep='/')
    print (destFile)
    if (!file.exists(destFile)) {
        download.file(url = fileURL, 
                      destfile = destFile)
        unzip(destFile)
    }
    
    NEI <- readRDS('summarySCC_PM25.rds')
    SCC <- readRDS('Source_Classification_Code.rds')
    
    plotFunc(NEI = NEI, SCC = SCC)
}

makePlot1 <- function(NEI, ...) {
    ## Plots the total emissions for 1999, 2002, 2005, and 2008
    
    years <- c(1999, 2002, 2005, 2008)
    ems <- c()
    for (year in years) {
        sub <- NEI[NEI$year == year,]
        ems <- c(ems, sum(sub$Emissions))
    }
    
    png(file = 'plot1.png')
    barplot(ems, 
            names.arg = years, 
            main = "Total emissions from PM2.5 1999-2008",
            xlab = 'Year',
            ylab = 'Total emissions (in tons)',
            col = 'red')
    dev.off()
}

getPMData(makePlot1)