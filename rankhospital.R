## This function produces a name of a hospital that is ranked at a given number

rankhospital <- function(state, outcome, num="best") {
        options(warn=-1) ## removing unneeded warnings
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character") ## loading data
        states <- factor(data[,7]) ## reads all possible states from the dataset
        st <- levels(states) ## creates a list of all available states
        if (sum(ifelse(state != st, FALSE, TRUE)) == 0) { ## tests if the state is ok 
                stop("invalid state") ## errors out
        }
        o <- c("heart attack", "heart failure", "pneumonia") ## setting outcomes
        if (sum(ifelse(outcome != o, FALSE, TRUE)) == 0) { ## testing if outcomes are ok
                stop("invalid outcome") ## errors out
        }
        stateData <- data[data$State==state,] ## selecting only needed state data
        if (outcome == "heart attack") { ## deciding which desease column to use
                col = 11
        } else if (outcome == "heart failure") {
                col = 17
        } else col = 23
        reportData <- stateData[,c(2,col)] ## skimming the data to the needed columns
        reportData[,2] <- as.numeric(reportData[,2]) ## getting figures of characters
        answer <- reportData[order(reportData[,2], reportData[,1]),] ## ordering data
        answer <- na.omit(answer) ## removing NAs
        count <- nrow(answer) ## setting maximum 'num' value
        if (num == "best") {
                answer[1,1] ## choose the first hospital's name
        } else if (num =="worst") {
                answer[count,1] ## get the last hospital's name
        } else if (num > col) {
                a = NA
                a ## get 'NA' result
        } else {
                answer[num,1] ## get rated result
        }
}