corr <- function (directory, threshold = 0) {
    ## Returns a vector of corrolations for the monitors that meet the
    ## threshold requirements
    completeSet <- complete (directory)
    completeBool <- completeSet [,2] > threshold
    id <- completeSet [completeBool, 1]
    
    corrs <- numeric (length (id))
    if (length (corrs) == 0){
        return (numeric(0))
    }
    
    counter <-1
    
    for (i in id) {
        fileName <- paste (directory, '/', formatC(i, width = 3, flag = '0'),
                           '.csv', sep ='')
        tempDf <- read.csv(fileName, header = TRUE)
        
        cc <- complete.cases(tempDf)
        corrs [counter] <- cor (tempDf$sulfate[cc], tempDf$nitrate[cc])
        
        counter = counter + 1
    }
    
    corrs
}

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

# cr <- corr("specdata")
# print (summary(cr))
# print (length(cr))
