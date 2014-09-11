rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character") 
  statename<-data[,7]
  ## states without NAs
  ##duplicate elements removed by unique
  statenew<-unique(statename[!is.na(statename)])
  ## order alphabetically
  state.order <- statenew[sort.list(statenew)]
  outcomename <- list(outcome.Index=c("heart attack","heart failure","pneumonia"),
                      state=state.order,h.index=list(heart.attack=11,heart.failure=17,pneumonia=23),
                      num.index=c("best","worst"))
  outcomename.Index <- which(outcomename$outcome.Index==outcome)
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  source("rankhospital.R")
  best.h <- sapply(outcomename$state,rankhospital,outcome,num)
  data.frame(hospital=best.h, state=outcomename$state)
}