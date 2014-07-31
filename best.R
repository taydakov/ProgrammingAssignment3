best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    data <- data[which(data$State == state & !is.na(data[colinterest]) & data[colinterest] != "Not Available"),]
    data[, colinterest] <- as.numeric(data[, colinterest])
    result <- data[which(data[colinterest] == min(data[colinterest])),]
    sorted <- result[order(result$Hospital.Name),]
    sorted$Hospital.Name[1]
}