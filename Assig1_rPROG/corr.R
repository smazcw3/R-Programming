corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    list_files_full <- list.files(directory,full.names=TRUE) ##get all file names
    df_poldata <- data.frame() ## initialise empty df to hold csv data
    vec_corr <- vector("numeric") ## initialise empty vector to hold correlation values
    i=1 ##counter to read in files
    j=1 ##counter to add correlation for monitors with complete.cases to vec_corr
    
    for (i in 1:332) {
        
        df_poldata <- rbind(df_poldata, read.csv(list_files_full[i], header = TRUE, sep = ",")) 
        vec_complete <- complete.cases(df_poldata)
        df_complete <- data.frame() ## initialise dataframe to hold only complete.cases
        nobs <- sum(vec_complete) ## number of complete cases
        
        if (nobs > threshold){
            df_complete <- df_poldata[vec_complete,] ## subset original data.frame to return only complete.cases
            vec_corr[j] <- cor(df_complete[2], df_complete[3], method = "pearson") ## add cor of columns 2 and 3 to vector
            j <- j+1 ## increment counter to ensure correlations are put in correct position in vector
        }
        df_poldata <- data.frame() ## clear dataframe
    }
    vec_corr
}