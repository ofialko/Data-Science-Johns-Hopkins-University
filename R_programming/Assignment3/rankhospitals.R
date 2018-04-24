rankhospital <- function(state, outcome, num) {
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
    } else if (outcome == 'heart failure') {
        rate <- split(data[,17],data$State)[[state]]
    } else if (outcome == 'pneumonia') {
        rate <- split(data[,23],data$State)[[state]]
    }
    
    rate <- as.numeric(rate)
    names <- hospitals[order(rate,hospitals)]
    ## removing NAs
    names <- names[!is.na(names)]
    
 
    
    if (num == 'best') {
        return(names[1])
    } else if (num == 'worst') {
        names <- hospitals[order(rate,hospitals,decreasing = T)]
        return(names[1])
    } else if (is.numeric(num)) {
        return(names[num])
    } else stop('num is not valid')

}
