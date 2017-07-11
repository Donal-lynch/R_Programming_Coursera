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
    # initialise the directory strign to match my directory structure
    filePrefix <- paste('./Week 2/rprog_data_specdata/',
                        directory, '/', sep ='')
    for (i in id){
        tempFileName <- paste (filePrefix,formatC(i, width = 3, flag = '0'),
                               '.csv', sep ='')
        tempDf <- read.csv(tempFileName, header = TRUE)
        
        pollMeans[counter] <- mean(tempDf[[pollutant]], na.rm = TRUE)
        pollCounts [counter] <- sum(complete.cases(tempDf[[pollutant]]))
        counter = counter + 1
    }
    sum (pollMeans*pollCounts)/sum(pollCounts)
}


complete <- function (directory, id = 1:332){
    # directory is a char vector indicating the file location
    # id is an int vector of the moitors to be insptected
    
    # return the num of complete observations
    # of all of the pollutant accross 'id' observations in a DF
    
    compCasesDf <- data.frame(matrix(ncol = 2, nrow = length (id)))
    colnames(compCasesDf) <- c("id", "nobs")
    counter <- 1
    
    filePrefix <- paste('./Week 2/rprog_data_specdata/',
                        directory, '/', sep ='')
    for (i in id) {
        tempFileName <- paste (filePrefix,formatC(i, width = 3, flag = '0'),
                               '.csv', sep ='')
        tempDf <- read.csv(tempFileName, header = TRUE)
        
        cc<- sum(complete.cases (tempDf))
        compCasesDf [counter,] <- c(i,cc)
        counter = counter + 1
    }
    compCasesDf
}
    

corr <- function (directory, threshold = 0) {
    ## Returns a vector of corrolations for the monitors that meet the
    ## threshold requirements
    
    counter <- 1
    filePrefix <- paste('./Week 2/rprog_data_specdata/',
                        directory, '/', sep ='')
    
    completeSet <- complete (directory)
    completeBool <- completeSet [,2] > threshold
    id <- completeSet [completeBool, 1]
    
    corrs <- numeric (length (id))
    if (length (corrs) == 0){
        return (numeric(0))
    }
    
    counter <-1
    
    for (i in id) {
        tempFileName <- paste (filePrefix,formatC(i, width = 3, flag = '0'),
                               '.csv', sep ='')
        tempDf <- read.csv(tempFileName, header = TRUE)
        
        #corrs [counter] <- 123
        cc <- complete.cases(tempDf)
        corrs [counter] <- cor (tempDf$sulfate[cc], tempDf$nitrate[cc])
            
            
        counter = counter + 1
        
    }
    corrs
}




#print(pollutantmean("specdata", "sulfate", 1:10))
#print(pollutantmean("specdata", "nitrate", 70:72))
#print(pollutantmean("specdata", "nitrate", 23))

# print(complete("specdata", 1))
# print(complete("specdata", c(2, 4, 8, 10, 12)))
# print(complete("specdata", 30:25))
# print(complete("specdata", 3))

# cr <- corr("specdata", 150)
# print (head(cr))
# print (summary(cr))
# 
# 
# cr <- corr("specdata", 400)
# print (head(cr))
# print (summary(cr))
# # 
# 
# cr <- corr("specdata", 5000)
# print (summary(cr))
# print (length(cr))

cr <- corr("specdata")
print (summary(cr))
print (length(cr))
