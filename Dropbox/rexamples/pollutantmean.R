pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV file
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    #initially there is no data
    data=numeric()
    for (i in id) {
    #list of file names
    file.names=paste(directory,"/",sprintf('%03d',i),'.csv',sep="")
    file=read.csv(file.names)
    data=c(data,file[[pollutant]])
    
    }
    # return mean 
    return(mean(data, na.rm=TRUE))
} 
##Alternative way to compute without for loop
#pollutantmean <- function(directory, pollutant, id = 1:332) {
#    data = lapply(id, function(i) read.csv(paste(directory, "/", sprintf('%03d',i),  '.csv', sep = ""))[[pollutant]])
    
#return(mean(unlist(data), na.rm = TRUE))
#}
#pollutantmean("specdata", "nitrate", 23)
