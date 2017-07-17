## This function takes the oucome and a rank and returns the hospital in each 
## state which ranks in tha poition based on that outcome. The output is a 2
## column data frame

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    rawData <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character")
    possOutcome <- c('heart attack', 'heart failure','pneumonia')
    
    ## Check that state and outcome are valid
    if (!any(possOutcome == outcome)) {
        stop ('invalid outcome')
    }
    
    
    
    
    ## For each state, find the hospital of the given rank
    # 11, 17, 23 respectively relate to possOutcome
    colInd <- sum((possOutcome == outcome) * c(11, 17, 23))
    # Hospital name is column 2, state is col 7
    # **********Hospital Name, State, Outcome**************
    colsOfInterest <- c(2, 7, colInd)
    dataSubSet <- subset (rawData, select = colsOfInterest)
    # The mortality rates are factors, but we need numerics. Converting straight
    # from factor to numeric gives strange numbers, hence the 2 stage conversion
    dataSubSet[,3] <- as.numeric(as.character(dataSubSet[,3]))
    # Remove NA rows
    dataSubSet <- dataSubSet[complete.cases(dataSubSet),]
    
    #Initialise the resultant df 
    allStates <- sort(unique (dataSubSet[,2]))
    NmStates <- length(allStates)
    result <- data.frame('hospital' = character(length = NmStates),
                         'state' =  allStates,
                         row.names = allStates,
                         stringsAsFactors = FALSE)
 
    for (state in allStates){
        HospsInStates <- dataSubSet [dataSubSet[,2] == state,]
        # ordering based on outcome first, alabetically second
        orderedVec <- HospsInStates[order(HospsInStates [,3],
                                           HospsInStates [,1]),]
        
        # This is inside the loop because worst depedends on the state
        HRank <- num
        validNum <- TRUE
        if (num == 'best'){
            HRank <- 1
        } else if (num == 'worst') {
            HRank <- nrow (orderedVec)
        } else if (num > nrow (orderedVec)) {
            validNum <- FALSE  
        }
        
        if (validNum) {
            result [state, 1 ]<- orderedVec[HRank, 1]
            result [state, 2 ]<- state
        } else {
            result [state, ]<- c(NA, state)
        }
        
        # if (state == 'WI'){
        #     print (orderedVec)
        # }
        
    }
    
    ## Return a data frame with the hospital names and the state name
    result
}
