## R Programming Assignment 3
## Alasdair GF
## https://github.com/AlasdairGF/RProgAss3

## Part Two (no part one)
best <- function(state="ZZ", outcome="blank") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
      ## read data and check if params passed to best() are valid
      ## consider moving this to a validate() function if identical
      ## requirements for the other 2 problems...
      fulldata <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
      data <- data.frame(fulldata[2],fulldata[7],fulldata[11],fulldata[17],fulldata[23])
      names(data) <- c("Hosp","State","ha","hf","pn")
      if (!state %in% data$State) stop("invalid state") # could optimise: 
            ## could optimise: 
            ## states <- levels(factor(data$State)) - this in an init fn?
            ## if (!state %in% states)
      if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
      
      ## ok quitting here for now.
      data<-data[data$State == state,] # choose only relev state
      if (outcome == "heart attack") data <- data[order(data$ha, decreasing=T),]
      if (outcome == "heart failure") data <- data[order(data$hf, decreasing=T),]
      if (outcome == "pneumonia")  data <- data[order(data$pn, decreasing=T),]
      
      as.character(data[1,1])
}

## Part Three
rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
}

## Part Four
rankall <- function(outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      ## (abbreviated) state name
}
