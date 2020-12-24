best <- function(state, outcome){
  ## Read outcome data
  outcome_care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- as.data.frame(cbind(outcome_care[, 2],   # hospital
                              outcome_care[, 7],   # state
                              outcome_care[, 11],  # heart attack
                              outcome_care[, 17],  # heart failure
                              outcome_care[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  ## Optional: data <- outcome_care[, c(2, 7, 11, 17, 23)]
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

  ## Check that state and outcome are valid
  if(!state %in% data[, "state"]){
    stop("invalid state")
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  # extract data of specific state
  extract_state <- data[which(data[, "state"] == state), ]  
  
  # coerce the 'outcome' column to numeric data type, suppress Warnings
  extract_state[, outcome] <- suppressWarnings(as.numeric(extract_state[, outcome]))
  
  # rank the rows by 'outcome' and hospital names
  sort_index <- order(extract_state[, outcome], extract_state[, 1], na.last = NA)  # get index
  extract_state_sorted <- extract_state[sort_index, ]  # re-order the rows
  
  # extract the hospital name of the first row
  hospital_name <- extract_state_sorted[1, 1] 
  print(hospital_name)
}