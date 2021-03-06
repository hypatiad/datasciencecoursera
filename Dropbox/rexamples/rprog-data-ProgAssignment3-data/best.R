best <- function(state, outcome) {
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
    state=state.order,h.index=list(heart.attack=11,heart.failure=17,pneumonia=23))
    
    if(!is.element(state,outcomename$state)){
    stop("invalid state")
    }
    if(!is.element(outcome,outcomename$outcome)){
    stop("invalid outcome")
    }
    
    ### using index outcome and " death rate column" are unified
    outcomename.Index <- which(outcomename$outcome.Index==outcome)
    ### somereason gets warnings so better to use suppressWarnings here
    h<- suppressWarnings(as.numeric(data[,outcomename$h.index[[outcomename.Index]]]))
    orderHospitalAll<-data[order(h,data[,2]),] #sort.list asks  partial=NULL, order is better to use
    orderHospitalAll[orderHospitalAll$State==state,][,2][1]
}