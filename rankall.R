## This function produces a data frame name of a hospital that is ranked at a given number

rankall <- function(outcome, num="best") {
        ##options(warn=-1) ## removing unneeded warnings
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character") ## loading data
        o <- c("heart attack", "heart failure", "pneumonia") ## setting outcomes
        if (sum(ifelse(outcome != o, FALSE, TRUE)) == 0) { ## testing if outcomes are ok
                stop("invalid outcome") ## errors out
        }
        if (outcome == "heart attack") { ## deciding which desease column to use
                col = 11
        } else if (outcome == "heart failure") {
                col = 17
        } else col = 23
        reportData <- data[,c(2,7,col)] ## skimming the data to the needed columns
        reportData[,3] <- as.numeric(reportData[,3]) ## getting figures of characters
        states <- factor(reportData[,2]) ## reads all possible states from the dataset
        st <- levels(states) ## creates a list of all available states
        rep <- 1:length(st) ## setting the number of repetitions for each state
        if (num == "best") {
                num = 1 ## choose the first hospital's name
        }
        count = 1 ## setting a counter for the task
        hospital <- vector() ##creating vector for list of hospital names
        for (i in rep) { ## setting a calculation for data.frame data
                state <- st[count] ## getting the value of the needed state for filtering
                temp <- reportData[reportData$State==state,] ## selecting only needed state data 
                temp <- na.omit(temp) ## removing NAs
                if (num =="worst") {
                        row <- nrow(temp)
                        answer <- temp[order(temp[,3], temp[,1]),]
                        hospital[count] <- answer[row,1] ## writing the name of a hospital
                } else if (num > nrow(temp)) {
                        hospital[count] <- NA ## setting NA value for missing rank
                } else {                
                        answer <- temp[order(temp[,3], temp[,1]),]
                        hospital[count] <- answer[num,1] ## writing the name of a hospital
                }
                count=count+1 ## giving a next value for counter
        }
        data.frame(hospital=hospital, state=st) ##create answer data.frame
}
 