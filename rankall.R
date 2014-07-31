rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    colinterest <- 0
    if (outcome == "heart attack")
        colinterest <- 11
    else if (outcome == "heart failure")
        colinterest <- 17 # heart failure
    else if (outcome == "pneumonia")
        colinterest <- 23
    else
        stop("invalid outcome")
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data <- data[which(!is.na(data[colinterest]) & data[colinterest] != "Not Available"),]
    data[, colinterest] <- as.numeric(data[, colinterest])
    data <- data[order(data$State, data[colinterest], data$Hospital.Name),]
    bystates <- split(data, data$State)

    result <- data.frame(hospital = vector(), state = vector())
    if (num == "best")
        num <- 1
    for (curr_state_id in 1:length(bystates)) {
        state <- bystates[[curr_state_id]]
        curr_num <- num
        if (num == "worst")
            curr_num <- nrow(state)
        if (curr_num > nrow(state))
            result <- rbind(result, data.frame(hospital = c(NA), state = c(state$State[1])))
        else
            result <- rbind(result, data.frame(hospital = c(state$Hospital.Name[curr_num]), state = c(state$State[1])))
    }
    
    result    
}