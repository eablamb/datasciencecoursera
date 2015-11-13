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

makePlot3 <- function(NEI, ...) {
    ## Compares Emissions by type in Baltimore for 1999, 2002, 2005, 2008
    
    require(ggplot2)
    
    sub <- NEI[NEI$fips == "24510",]
    png(file = 'plot3.png')
    p = qplot(x = year, 
              y = Emissions, 
              data = sub, 
              facets = type ~ ., 
              geom = 'bar', 
              stat = 'identity',
              main = "Emissions by Type in Baltimore City (1999-2008)")
    print(p)
    dev.off()
}

getPMData(makePlot3)