## R Programming Assignment 3
## Alasdair GF
## https://github.com/AlasdairGF/RProgAss3

## Part Three
rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      
      fulldata <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
      data <- data.frame(fulldata[2],fulldata[7],fulldata[11],fulldata[17],fulldata[23])
      names(data) <- c("Hosp","State","ha","hf","pn")
      if (!state %in% data$State) stop("invalid state") 
      if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) stop("invalid outcome")
      
      data<-data[data$State == state,] # choose only relev state
      if (outcome == "heart attack") data <- data[order(data$ha,data$Hosp),]
      if (outcome == "heart failure") data <- data[order(data$hf,data$Hosp),]
      if (outcome == "pneumonia")  data <- data[order(data$pn,data$Hosp) & !is.na(data$pn),] ##
      
      if (num=="best") num <- 1
      if (num=="worst") num <- nrow(data)
      as.character(data[num,1])
      #tail(data,10)
}
