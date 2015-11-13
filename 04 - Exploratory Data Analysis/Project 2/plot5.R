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

makePlot5 <- function(NEI, SCC) {
    ## Plots emmissions from motor vehicles in Baltimore 1999-2008
    
    SCCMotor <- SCC[grepl('Mobile - On-Road.*',SCC$EI.Sector),]
    NEIMotorBaltimore <- NEI[NEI$SCC %in% SCCMotor$SCC & NEI$fips == "24510",]
    png(file = 'plot5.png')
    p = qplot(x = year,
              y = Emissions,
              data = NEIMotorBaltimore,
              geom = 'bar',
              stat = 'identity',
              main = 'Motor vehicle emissions in Baltimore (1999-2008)')
    print(p)
    dev.off()
}

getPMData(makePlot5)