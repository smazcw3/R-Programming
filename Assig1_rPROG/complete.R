complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    files_list <- list.files(directory, full.names = TRUE)
    nobs <- NULL
    for(i in id){
        nobs <- c(nobs, nrow(na.omit(read.csv(files_list[i]))))
        DF <- data.frame(cbind(id, nobs))
    }
    DF
}

