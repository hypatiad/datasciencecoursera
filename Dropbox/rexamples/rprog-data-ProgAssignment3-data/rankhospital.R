rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv",colClasses = "character") 
    statename<-data[,7]
    ## states without NAs
    ##duplicate elements removed by unique
    statenew<-unique(statename[!is.na(statename)])
    ## order alphabetically
    state.order <- statenew[sort.list(statenew)]
    ## Check that state and outcome are valid
    outcomename <- list(outcome.Index=c("heart attack","heart failure","pneumonia"),
    state=state.order,h.index=list(heart.attack=11,heart.failure=17,pneumonia=23),
    num.index=c("best","worst"))
    ## Check that state and outcome are valid
    if(!is.element(state,outcomename$state)){
    stop("invalid state")
    }
    if(!is.element(outcome,outcomename$outcome)){
    stop("invalid outcome")
    }
    ### using index outcome and " death rate column" are unified
    outcomename.Index <- which(outcomename$outcome.Index==outcome)
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    h<- suppressWarnings(as.numeric(data[,outcomename$h.index[[outcomename.Index]]]))
    # reverse sorting to get the worst hospital
    h <- ifelse(rep(num==outcomename$num.index[2], length(h)),-h,h)
    num <- ifelse(is.numeric(num),num,1)
    orderHospitalAll<-data[order(h,data[,2]),] #sort.list asks  partial=NULL, order is better to use
    if (num > length(h) ) {
    return(NA)
    } 
    else if(num=="best"){
    r<-1}
    else if(num=="worst"){
    r<-length(h)}
    else if (num <= length(h) ) {
    r <- num
    } 
    return(orderHospitalAll[orderHospitalAll$State==state,][,2][r])
   
    
}

    
    
    
    
    

