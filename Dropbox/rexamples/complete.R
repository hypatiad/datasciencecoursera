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
    nobs=numeric()
    for (i in id){
    file.names=paste(directory,"/",sprintf('%03d',i),'.csv',sep="")
    file=read.csv(file.names)
    my_data<-complete.cases(file)
    nobs=c(nobs,sum(my_data))   
}
    return(data.frame(id,nobs))
}

##Alternative
#complete <- function(directory, id = 1:332) {
#f <- function(i) {
# data = read.csv(paste(directory, "/", sprintf('%03d',i),  '.csv', sep = ""))
#sum(complete.cases(data))
# }
# nobs = sapply(id, f)
#return(data.frame(id, nobs))