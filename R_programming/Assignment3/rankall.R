rankall <- function(outcome, num='best') {
    ## Read outcome data
    options(warn = -1)
    data <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')
    
    outcomes = c('heart attack','heart failure','pneumonia')
    if (!(outcome %in% outcomes)) {
        stop('invalid outcome')
    }
    
    
    if (outcome == 'heart attack') {
        data[,11] <- as.numeric(data[,11])
        data_state <- split(data[,c(2,11,7)],data$State)
    } else if (outcome == 'heart failure') {
        data[,17] <- as.numeric(data[,17])
        data_state <- split(data[,c(2,17,7)],data$State)
    } else if (outcome == 'pneumonia') {
        data[,23] <- as.numeric(data[,23])
        data_state <- split(data[,c(2,23,7)],data$State)
    }

    data_ordered <- lapply(data_state, function(x) x[order(x[,2],x[,1]),c(1,3)])

    ## removing NAs
    ## names <- names[!is.na(names)]
    
    
    
    if (num == 'best') {
        vec<-sapply(data_ordered,function(x) x[1,1])
        return(data.frame(hospital=vec,state=names(vec)))
    } else if (num == 'worst') {
        data_ordered <- lapply(data_state, function(x) x[order(x[,2],x[,1],decreasing = T),c(1,3)])
        vec<-sapply(data_ordered,function(x) x[1,1])
        return(data.frame(hospital=vec,state=names(vec)))
    } else if (is.numeric(num)) {
        vec<-sapply(data_ordered,function(x) x[num,1])
        return(data.frame(hospital=vec,state=names(vec)))
    } else stop('num is not valid')
}
