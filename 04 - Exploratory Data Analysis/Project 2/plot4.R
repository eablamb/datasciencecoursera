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

makePlot4 <- function(NEI, SCC) {
    ## Plots Total U.S. Coal-based emissions 1999-2008
    
    require(ggplot2)
    
    SCCCoal <- SCC[grepl('.*Coal',SCC$EI.Sector),]
    NEICoal <- NEI[NEI$SCC %in% SCCCoal$SCC,]
    png(file = 'plot4.png')
    p = qplot(x = year,
              y = Emissions,
              data = NEICoal,
              geom = 'bar',
              stat = 'identity',
              main = 'U.S. Coal emissions (1999-2008)')
    print(p)
    dev.off()
}

getPMData(makePlot4)