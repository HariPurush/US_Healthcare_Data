## Provides the hospital for a given state and mortality (outcome)
## that has the ranking (num)

## It can also return the "best" or "worst" hospital for a given state
## rankhospital("MD", "heart failure", "best")

## rankhospital("MD", "heart failure", 5) : returns 5th best hospital for
## lowest heart failure mortality in MD

## Ranking ties will be handled alphabetically by hospital name

rankhospital <- function(state, outcome, num = "best") {
    # Raw data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    # Matches the "outcome" to one in the csv file
    outcome <- gsub(" ", ".", outcome, fixed = TRUE)
    pattern <- paste('^Hospital.30.Day.Death..Mortality..Rates.from.', outcome, sep="")
    a <- grepl(toupper(pattern), toupper(colnames(data)), ignore.case = TRUE)
    
    outcome_name <- (colnames(data)[a])
    state_data <- data[data$State == state, c("Hospital.Name", outcome_name)]
    
    # Error handling for invalid outcome or state
    if (!(outcome == "heart.failure" || outcome == "pneumonia" || outcome == "heart.attack")) {
        stop("invalid outcome")
    }
    if (nrow(state_data) == 0) {
        stop("invalid state")	
    }

    # Converts data from character to numeric for sorting
    state_data[,2] <- suppressWarnings(as.numeric(state_data[,2]))
    
    # Data is ordered by outcome from first to last and ties broken by hospital name
    ordered_state_data <- order(state_data[outcome_name], state_data$Hospital.Name, na.last=NA)
    
    # if "best" returns first result, "worst" returns last, otherwise returns the "num" result
    if (num == "best") {
        print(as.character(state_data$Hospital.Name[ordered_state_data[1]]))
    } else if (num == "worst") {
        print(as.character(state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]))
    } else if (is.numeric(num)) {
        print(as.character(state_data$Hospital.Name[ordered_state_data[num]]))
    } else {
        stop("invalid num")
    }
}