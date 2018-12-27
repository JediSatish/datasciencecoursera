setwd("F:/Britannia/data_science/coursera/John_Hopkins/R_programming/Week_4")

rankall <- function( outcome, num = "best") 
{
  #Read the input file
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #get the outcome names
  data_outcome <- c("HEART ATTACK","HEART FAILURE","PNEUMONIA")
  #outcome <- "HEART ATTACK"
  outcome <- toupper(outcome)
  data_state <- unique(outcome_data$State)
  if (outcome %in% data_outcome) 
  {
    
    if(outcome == "HEART ATTACK") {
outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    rank_data <- outcome_data[,c(2,7,11)]
    rank_data <- rank_data[complete.cases(rank_data),]
    rank_data <- rank_data[(order(rank_data[,2],rank_data[,3],rank_data[,1])),]
    }
    else if(outcome == "HEART FAILURE") {
      outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      rank_data <- outcome_data[,c(2,7,17)]
      rank_data <- rank_data[complete.cases(rank_data),]
      rank_data <- rank_data[(order(rank_data[,2],rank_data[,3],rank_data[,1])),]
    }
    else  {
      outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      rank_data <- outcome_data[,c(2,7,23)]
      rank_data <- rank_data[complete.cases(rank_data),]
      rank_data <- rank_data[(order(rank_data[,2],rank_data[,3],rank_data[,1])),]
    }
    split_data <- split(rank_data,rank_data$State)
    #class(split_data)
    #str(split_data)
    #summary(split_data)
    ans = lapply(split_data, function(x, num) {
      if(toupper(num)=="BEST")
      {
        return(x$Hospital.Name[1])
      }
      else if (toupper(num) =="WORST")
      {
        return(x$Hospital.Name[nrow(x)])
      }
      else
      {
        return(x$Hospital.Name[num])
      }
    
  },num)
    return ( data.frame(hospital=unlist(ans), state=names(ans)) )
    
  }
}

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)