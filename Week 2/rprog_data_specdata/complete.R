complete <- function (directory, id = 1:332){
    # directory is a char vector indicating the file location
    # id is an int vector of the moitors to be insptected
    
    # return the num of complete observations
    # of all of the pollutant accross 'id' observations in a DF
    
    compCasesDf <- data.frame(matrix(ncol = 2, nrow = length (id)))
    colnames(compCasesDf) <- c("id", "nobs")
    counter <- 1
    
    for (i in id) {
        fileName <- paste (directory, '/', formatC(i, width = 3, flag = '0'),
                           '.csv', sep ='')
        tempDf <- read.csv(fileName, header = TRUE)
        
        compCasesDf [counter,] <- c(i, sum(complete.cases (tempDf)))
        counter = counter + 1
    }
    compCasesDf
}

# print(complete("specdata", 1))
# print(complete("specdata", c(2, 4, 8, 10, 12)))
# print(complete("specdata", 30:25))
# print(complete("specdata", 3))
# print(complete("specdata", 275))