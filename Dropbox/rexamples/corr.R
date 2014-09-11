corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    
    ## store the completely observed cases
    newfile=complete(directory,id=1:332)
    ##gives nobs,which is the number of completely observed cases
    
    ## ind:corresponding id number for the ones greater than threshold
    ind=newfile[newfile["nobs"]>threshold,]$id
    crr=numeric()
    for (i in ind) {
    file.names=paste(directory,"/",sprintf('%03d',i),'.csv',sep="")
    file=read.csv(file.names)
    my_data=file[complete.cases(file),]
    crr=c(crr,cor(my_data[['sulfate']],my_data[['nitrate']]))
    }
    # return correlation 
    return(crr)
}