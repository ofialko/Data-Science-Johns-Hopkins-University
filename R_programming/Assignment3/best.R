best <- function(state, outcome) {
    ## Read outcome data
    options(warn = -1)
    data <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')
    ## Check that state and outcome are valid
    if (!(state %in% unique(data[,'State']))) {
        stop('invalid state')
    }
    outcomes = c('heart attack','heart failure','pneumonia')
    if (!(outcome %in% outcomes)) {
        stop('invalid outcome')
    }
    
    hospitals <- split(data$Hospital.Name,data$State)[[state]]
    if (outcome == 'heart attack') {
        rate <- split(data[,11],data$State)[[state]]
        rate <- as.numeric(rate)
        minval <- min(rate,na.rm = T)
        names <- unique(hospitals[rate==minval])
        name <- sort(names,na.last = T)[1]
    } else if (outcome == 'heart failure') {
        rate <- split(data[,17],data$State)[[state]]
        rate <- as.numeric(rate)
        minval <- min(rate,na.rm = T)
        names <- unique(hospitals[rate==minval])
        name <- sort(names,na.last = T)[1]
    } else if (outcome == 'pneumonia') {
        rate <- split(data[,23],data$State)[[state]]
        rate <- as.numeric(rate)
        minval <- min(rate,na.rm = T)
        names <- unique(hospitals[rate==minval])
        name <- sort(names,na.last = T)[1]   
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    return(name)
}
