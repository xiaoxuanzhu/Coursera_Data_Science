rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- outcome_care[, c(2, 7, 11, 17, 23)]
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that outcome is valid
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  states_all <- sort(unique(data[, 2]))  # get all the states names and sort
  output_all <- data.frame()  # empty data frame
  
  ## For each state, find the hospital of the given rank
  for (i in 1:length(states_all)) {
    state_extracted <- data[which(data[, "state"] == states_all[i]), ]
    state_extracted[, outcome] <- suppressWarnings(as.numeric(state_extracted[, outcome]))
    index_order <- order(state_extracted[, outcome], state_extracted[, 1], na.last = NA)
    data_ordered <- state_extracted[index_order, ]
    
    if (num == "best") {
      output_state <- data_ordered[1, 1:2]
    } else if (num == "worst") {
      output_state <- data_ordered[nrow(data_ordered), 1:2]
    } else if (num > nrow(data_ordered)) {
      output_state <- data.frame("hospital" = NA, "state" = states_all[i])
    } else {
      output_state <- data_ordered[num, 1:2]
    }
    
    ## append data frames together
    output_all <- rbind(output_all, output_state)
  }
  
  output_all
}