rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    ## Check that state and outcome are valid
    if (!(state %in% data$State))
        stop("invalid state")
    colinterest <- 0
    if (outcome == "heart attack")
        colinterest <- 11
    else if (outcome == "heart failure")
        colinterest <- 17 # heart failure
    else if (outcome == "pneumonia")
        colinterest <- 23
    else
        stop("invalid outcome")
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    data <- data[which(data$State == state & !is.na(data[colinterest]) & data[colinterest] != "Not Available"),]
    data[, colinterest] <- as.numeric(data[, colinterest])
    sorted <- data[order(data[colinterest], data$Hospital.Name),]
    if (num == "best")
        num <- 1
    else if (num == "worst")
        num <- length(sorted[, 1])
    if (num > length(sorted[, 1]))
        NA
    else 
        sorted$Hospital.Name[num]
}