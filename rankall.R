## R Programming Assignment 3
## Alasdair GF
## https://github.com/AlasdairGF/RProgAss3

## Part Four
rankall <- function(outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
      
      # Read data & validate query
      fulldata <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
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
      
      # Initialise empty data.frame for results
      results <- data.frame(hospital=character(),state=character(),stringsAsFactors=FALSE)
      
      states <- levels(factor(data$State)) # vector of states
      
      # translate "best" and "worst" to row numbers
      if (num=="best") num <- 1
      if (num=="worst") num <- nrow(data)
      
      for (loop in seq(length(states))) {
            tempData <- data[data$State == states[loop],] # select each state in turn
            tempData <- tempData[order(tempData$rate,tempData$Hosp),] # order it
            newRow <- data.frame(hospital=as.character(tempData$Hosp[num]),state=as.character(tempData$State[num]))
            colnames(newRow)<-colnames(results)
            results <- rbind(results,newRow) # add
      }
      colnames(results)<-c("hospital","state")
      results
}
