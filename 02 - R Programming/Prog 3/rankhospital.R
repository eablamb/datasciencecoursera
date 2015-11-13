setwd('C:/Users/eugene_lamb/Projects/Data Science/R Programming/Prog 3/')

rankhospital <- function(state, outcome, num = "best") {
    
    getRankedEntry <- function(df, x, n) {
        
        ## First coerce the `x` column into a numeric
        suppressWarnings(df[[x]] <- as.numeric(as.character((df[[x]]))))
        
        ## Remove rows with NA values
        df <- df[!is.na(df[[x]]),]
        
        ## Handle 'best' and 'worst'
        if (n == 'best') {
            n <- 1
        }
        
        if (n == 'worst') {
            n <- nrow(df)
        }
        
        ## Sort by outcome and hospital name, then get the `n`th best
        retVal <- as.character(df[order(df[[x]],df[[1]]),][n,1])
        
        ## All return values must be assigned to a variable
        retVal
    }
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    colNames <- c('hospital',
                  'heart attack',
                  'heart failure',
                  'pneumonia')
    
    ## Check that state and outcome are valid
    if (!(state %in% data$State)) {
        stop("invalid state")
    }
    
    if (!(outcome %in% colNames[2:4])) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    candidates <- data[data$State == state, c(2,11,17,23)]
    names(candidates) <- colNames
    nthBestHospital <- getRankedEntry(candidates, outcome, num)
    
    nthBestHospital
}