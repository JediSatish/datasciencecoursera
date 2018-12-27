
#getwd()
#list.files()
#head(outcome)
#str(outcome)
#colnames(outcome_data)

best <- function(state, outcome) 
  {
  
  #Read the input file
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #get the unique state names
  data_state <- unique(outcome_data$State)
  #get the outcome names
  data_outcome <- c("HEART ATTACK","HEART FAILURE","PNEUMONIA")
  #"AL" %in% data_state
  #state <- "MD"
  #outcome <- "HEART ATTACK"
  state <- toupper(state)
  outcome <- toupper(outcome)
  
  #outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  #outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  #outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
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
    state_split_data[1,1]
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
    state_split_data[1,1]
  }
  else 
  {
    #Extract the hospital name and the mortality rate for last 30 days for the outcome
    state_split_data<-outcome_data[(outcome_data$State ==state) ,c(2,23)]
    #The below conversion is done to remove the rows without any numeric records.  This also introduces NAs (implicit coercion)
    state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(state_split_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    #remove NAs
    state_split_data <- state_split_data[complete.cases(state_split_data),]
    #summary(state_split_data)
    #str(state_split_data)
    state_split_data <- state_split_data[(order(state_split_data[,2],state_split_data[,1])), ]
    state_split_data[1,1]
    
  }

  
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

}

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

#best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

