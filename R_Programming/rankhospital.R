rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- outcome_care[, c(2, 7, 11, 17, 23)]
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% data[, "state"]){
    stop("invalid state")
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  state_extracted <- data[which(data[, "state"] == state), ]
  state_extracted[, outcome] <- suppressWarnings(as.numeric(state_extracted[, outcome]))
  index_order <- order(state_extracted[, outcome], state_extracted[, 1], na.last = NA)
  data_ordered <- state_extracted[index_order, ]
  if (num == "best") {
    print(data_ordered[1, 1])
  } else if (num == "worst") {
    print(data_ordered[nrow(data_ordered), 1])
  } else if (num > nrow(data_ordered)) {
    print(NA)
  } else {
    print(data_ordered[num, 1])
  }
}