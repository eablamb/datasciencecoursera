##EDA COURSE - PROJECT 2 OCT 2014
##PM 2.5 EMISSIONS FROM MOTOR VEHICLE BALTIMORE vs LA 1999 TO 2008
##------------------------------------------------------------------------------
emissionMotorBaltvsLA <- function(){
    
    ##check if the file exists in the current work directory
    if ((file.exists("summarySCC_PM25.rds")==FALSE)|(file.exists("Source_Classification_Code.rds")==FALSE)) {
        ##stop the function if the data file does not exist
        errorMessage <- paste('Error in plot creation, please make sure that summarySCC_PM25.rds and Source_Classification_Code.rds are saved in your working directory with the correct file names!')
        stop(errorMessage)
    }
    
    ## display user message
    print('Plot is being created')
    
    ##open and read data files
    df_rawdata <- readRDS('summarySCC_PM25.rds')
    df_source <- readRDS('Source_Classification_Code.rds')
    
    ##identify the motor vehicle sources from EI.Sector
    df_vehicle <- subset(df_source,grepl('Vehicle',df_source$EI.Sector))
    vec_vehicle <- as.character(df_vehicle[,1])
    
    ## filter the main data set to get baltimore and LA records and then only motor vehicle sources
    df_cleandata <- subset(df_rawdata,df_rawdata$fips %in% c('24510','06037'))
    df_cleandata <- subset(df_cleandata,df_cleandata$SCC %in% vec_vehicle)
    
    ## aggreate data to sum up the PM2.5 emissions for these specific sources
    df_cleandata <- aggregate(df_cleandata$Emissions,by=list(df_cleandata$fips,df_cleandata$year),FUN=sum)
    colnames(df_cleandata) <- c('City','Year','Emissions')
    ##create and save the plot
    ggplot(df_cleandata,aes(x=as.numeric(Year),y=Emissions,colour=City))+geom_line(size=0.7)+
        geom_point(size = 0.5, col='orange') +
        ggtitle('PM2.5 Emissions for motor vehicle - Baltimore City Vs LA from  1999 to 2008') +
        xlab('Years') +
        ylab('PM 2.5 emissions') +
        labs(aesthetic='City')+
        theme(text = element_text(size=4))
    
    ggsave('PM25 Emissions Baltimore VS LA Motor vehicle.png',height=3,width=3)
    
    
}