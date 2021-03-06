## R Programming Assignment 3
## Alasdair GF
## https://github.com/AlasdairGF/RProgAss3

## Part Three
rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      # Read data & validate query
      fulldata <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
      if (!state %in% data$State) stop("invalid state") 
      if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
      
      # Select out only the data needed for this problem...
      if (outcome=="heart attack") {
            data <- data.frame(fulldata[2],fulldata[7],fulldata[11])
      } else if (outcome=="heart failure") {
            data <- data.frame(fulldata[2],fulldata[7],fulldata[17])
      } else {
            data <- data.frame(fulldata[2],fulldata[7],fulldata[23])
      }
      names(data) <- c("Hosp","State","rate")
      data<-data[complete.cases(data$rate),] # strip out all rows where rate is NA
      data<-data[data$State == state,] # choose only relev state
      
      # order data
      data <- data[order(data$rate,data$Hosp),]
      
      # translate "best" and "worst" to row numbers
      if (num=="best") num <- 1
      if (num=="worst") num <- nrow(data)

      # Return relevant hosp name
      as.character(data[num,1])
}