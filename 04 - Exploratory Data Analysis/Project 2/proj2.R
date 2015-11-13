# Exploratory Data Analysis: Project #2

setwd('C:/Users/eugene_lamb/Projects/Data Science/Exploratory Data Analysis/Project 2')

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
    
    barplot(ems, 
            names.arg = years, 
            main = "Total emissions from PM2.5 1999-2008",
            xlab = 'Year',
            ylab = 'Total emissions (in tons)',
            col = 'red')
}

makePlot2 <- function(NEI, ...) {
    ## Plots total emissions in Baltimore for 1999-2008
    
    years <- c(1999, 2002, 2005, 2008)
    ems <- c()
    for (year in years) {
        sub <- NEI[NEI$year == year & NEI$fips == "24510",]
        ems <- c(ems, sum(sub$Emissions))
    }
     
    barplot(ems, 
            names.arg = years, 
            main = "Total emissions from PM2.5 in Baltimore City 1999-2008",
            xlab = 'Year',
            ylab = 'Total emissions (in tons)',
            col = 'blue')
}

makePlot3 <- function(NEI, ...) {
    ## Compares Emissions by type in Baltimore for 1999, 2002, 2005, 2008

    require(ggplot2)
    
    sub <- NEI[NEI$fips == "24510",]
    qplot(x = year, 
          y = Emissions, 
          data = sub, 
          facets = type ~ ., 
          geom = 'bar', 
          stat = 'identity',
          main = "Emissions by Type in Baltimore City (1999-2008)") 
}

makeplot4 <- function(NEI, SCC) {
    ## Plots Total U.S. Coal-based emissions 1999-2008
    
    require(ggplot2)
    
    SCCCoal <- SCC[grepl('.*Coal',SCC$EI.Sector),]
    NEICoal <- NEI[NEI$SCC %in% SCCCoal$SCC,]
    qplot(x = year,
          y = Emissions,
          data = NEICoal,
          geom = 'bar',
          stat = 'identity',
          main = 'U.S. Coal emissions (1999-2008)')
}

makeplot5 <- function(NEI, SCC) {
    ## Plots emmissions from motor vehicles in Baltimore 1999-2008
    
    SCCMotor <- SCC[grepl('Mobile - On-Road.*',SCC$EI.Sector),]
    NEIMotorBaltimore <- NEI[NEI$SCC %in% SCCMotor$SCC & NEI$fips == "24510",]
    qplot(x = year,
          y = Emissions,
          data = NEIMotorBaltimore,
          geom = 'bar',
          stat = 'identity',
          main = 'Motor vehicle emissions in Baltimore (1999-2008)')
}

makeplot6 <- function(NEI, SCC) {
    ## Compare emissions from motor vehicles between Baltimore and Los Angeles
    ## (1999-2008)
    
    SCCMotor <- SCC[grepl('Mobile - On-Road.*',SCC$EI.Sector),]
    NEIMotorCompare <- NEI[NEI$SCC %in% SCCMotor$SCC & 
                               (NEI$fips == "24510" | NEI$fips == "06037"),]
    qplot(x = year,
          y = Emissions,
          data = NEIMotorCompare,
          facets = fips ~ ., 
          geom = 'bar',
          stat = 'identity',
          main = 'Comparison of Motor vehicle emissions in for 24510 (Baltimore) and 06037 (LA) 1999-2008')
}