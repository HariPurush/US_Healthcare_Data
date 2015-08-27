best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    outcome <- gsub(" ", ".", outcome, fixed = TRUE)
    pattern <- paste('^Hospital.30.Day.Death..Mortality..Rates.from.', outcome, sep="")
    a <- grepl(toupper(pattern), toupper(colnames(data)), ignore.case = TRUE)

    ## (colnames(data)[a]) produces the column name
    ## which(a) produces the associated column number
    outcome_name <- (colnames(data)[a])
    state_data <- subset(data, data[, 7] == state)
    
    if (state %in% state_data$State == FALSE){ stop ('invalid state')}
    if (outcome_name %in% colnames(data) == FALSE){ stop ('invalid outcome')}
    
    outcome_data <- state_data[,which(a)]

    min_row <- suppressWarnings(which.min(outcome_data))
    print(state_data[min_row, 2])
}