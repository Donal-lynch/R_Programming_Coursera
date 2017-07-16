## This function takes a state, outcome and rank as args
## and returund the hosipital in that state which ranks 
## in the rank location in terms of outcome

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    rawData <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character")
    possOutcome <- c('heart attack', 'heart failure','pneumonia')
    
    ## Check that state and outcome are valid
    # Check that state and outcome are valid
    if (!any(rawData$State == state)) {
        stop ('invalid state')
    }
    if (!any(possOutcome == outcome)) {
        stop ('invalid outcome')
    }
    
    # 11, 17, 23 respectively relate to possOutcome
    colInd <- sum((possOutcome == outcome) * c(11, 17, 23))
    # Hospital name is column 2
    colsOfInterest <- c(2, colInd)
    rowsOfInterest <- (rawData ['State'] == state)
    dataSubSet <- subset (rawData, rowsOfInterest, colsOfInterest)
    # The mortality rates are factors, but we need numerics. Converting straight
    # from factor to numeric gives strange numbers, hence the 2 stage conversion
    dataSubSet[,2] <- as.numeric(as.character(dataSubSet[,2]))
    #Remove the NA rows
    dataSubSet <- dataSubSet[complete.cases(dataSubSet),]
    
    
    if (num == 'best'){
        num <- 1
    } else if (num == 'worst') {
        num <- nrow (dataSubSet) 
    } else if (num > nrow (dataSubSet)) {
        return (NA)
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    orderedList <- dataSubSet[order(dataSubSet [,2], dataSubSet [,1]),]
    orderedList [num, 1]
}
