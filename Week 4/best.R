## This funbtion takes an abreviated state name (2 chars) and a disease as 
## arguments and returns a character vector with the name of the best hospital
## in that state with respect to the 30 day mortality of the given disease

best <- function(state, outcome) {
    ## Read outcome data
    rawData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    possOutcome <- c('heart attack', 'heart failure','pneumonia')
    
    ## Check that state and outcome are valid
    if (!any(rawData$State == state)) {
        stop ('invalid state')
    }
    if (!any(possOutcome == outcome)) {
        stop ('invalid outcome')
    }
    
    # Names of the columns are long, so applying col index based on 
    # outcome manually
    # Numbers are the relevant cols related to each outcome
    # The result of the == will be a vector with two F's and a T
    # so multiplying it by the vector gives two 0's and the corect number
    # the sum simply converts this numeric(3) to numeric (1)
    
    colInd <- sum((possOutcome == outcome) * c(11, 17, 23))
    # Hospital name is column 2
    colsOfInterst <- c(2, colInd)
    dataSubSet <- subset (rawData, rawData ['State'] == state, colsOfInterst)
    # The mortality rates are currently factors, but we need numerics
    # Converting straight from factor to numeric gives strange numbers, 
    # hence the 2 stage conversion
    dataSubSet[,2] <- as.numeric(as.character(dataSubSet[,2]))      
    
    bestOutcome <- min (dataSubSet[,2], na.rm = TRUE)
    # == operation produces a vector with all best hospitals in case of a tie
    # the sort is use to alphbetise the list
    bestHosp <- sort(dataSubSet[,1][dataSubSet[,2] == bestOutcome])
    bestHosp [1]
    
}