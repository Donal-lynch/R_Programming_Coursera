pollutantmean <- function (directory, pollutant, id = 1:332){
    # directory is a char vector indicating the file location
    # pollutant is a char vector - either 'sulfate' or 'nitrate'
    # id is an int vector of the moitors to be insptected
    
    # return the mean of all of the pollutant accross 'id' observations
    
    if (pollutant != 'sulfate' & pollutant != 'nitrate'){
        stop ('Pollutant must be either \'sulfate\'  \'nitrate\'')
    }
    
    # Initialise a variable for the means
    pollMeans <- numeric (length (id))
    pollCounts <- numeric (length (id))
    counter <- 1
    
    # Read in the data using a for loop
    for (i in id){
        fileName <- paste (directory, '/', formatC(i, width = 3, flag = '0'),
                           '.csv', sep ='')
        tempDf <- read.csv(fileName, header = TRUE)
        
        pollMeans[counter] <- mean(tempDf[[pollutant]], na.rm = TRUE)
        pollCounts [counter] <- sum(complete.cases(tempDf[[pollutant]]))
        counter = counter + 1
    }
    sum (pollMeans*pollCounts)/sum(pollCounts)
}

print(pollutantmean("specdata", "sulfate", 1:10))
print(pollutantmean("specdata", "nitrate", 70:72))
print(pollutantmean("specdata", "nitrate", 23))
