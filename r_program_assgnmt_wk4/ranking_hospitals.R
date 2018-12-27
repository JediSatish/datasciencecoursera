

rankhospital <- function(state, outcome, num = "best") 
  {
  #Read the input file
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #get the unique state names
  data_state <- unique(outcome_data$State)
  #get the outcome names
  data_outcome <- c("HEART ATTACK","HEART FAILURE","PNEUMONIA")
  #"AL" %in% data_state
  state <- "TX"
  outcome <- "PNEUMONIA"
  state <- toupper(state)
  outcome <- toupper(outcome)
  if (state %in% data_state && outcome %in% data_outcome) 
  {
    if(outcome=="HEART ATTACK")
    {
      #Extract the hospital name and the mortality rate for last 30 days for the outcome
      state_split_data<-outcome_data[(outcome_data$State ==state) ,c(2,11)]
      #The below conversion is done to remove the rows without any numeric records.  This also introduces NAs (implicit coercion)
      state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
      #remove NAs
      state_split_data <- state_split_data[complete.cases(state_split_data),]
      #summary(state_split_data)
      #str(state_split_data)
      state_split_data <- state_split_data[(order(state_split_data[,2],state_split_data[,1])), ]
     
    }
    
    else if(outcome=="HEART FAILURE") 
    {
      #Extract the hospital name and the mortality rate for last 30 days for the outcome
      state_split_data<-outcome_data[(outcome_data$State ==state) ,c(2,17)]
      #The below conversion is done to remove the rows without any numeric records.  This also introduces NAs (implicit coercion)
      state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      #remove NAs
      state_split_data <- state_split_data[complete.cases(state_split_data),]
      #summary(state_split_data)
      #str(state_split_data)
      state_split_data <- state_split_data[(order(state_split_data[,2],state_split_data[,1])), ]
      #state_split_data[1,1]
    }
    else 
    {
      #Extract the hospital name and the mortality rate for last 30 days for the outcome
      state_split_data<-outcome_data[(outcome_data$State ==state) ,c(2,23)]
      #The below conversion is done to remove the rows without any numeric records.  This also introduces NAs (implicit coercion)
      state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      #remove NAs
      state_split_data <- state_split_data[complete.cases(state_split_data),]
      state_split_data <- state_split_data[(order(state_split_data[,2],state_split_data[,1])), ]
      #state_split_data[1,1]
      
    }
    if(num =="best")
    {
      num <-1
    }
    else if (num=="worst")
    {
      num <- nrow(state_split_data)
    }
    else
    {
      num <- as.numeric(num)
    }
    return(state_split_data[num,1])
  }
  
  else  
  {
    if(!(state %in% data_state)) 
    {
      stop(print("INVALID STATE :   Please provide correct variable"))
    }
    else 
    {
      stop(print("INVALID OUTCOME has been passed. Please provide correct variable"))
    }
   
  }
  
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
