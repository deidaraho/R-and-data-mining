best <- function(state, outcome) {
## Read outcome data
	file <- read.csv('outcome-of-care-measures.csv',colClasses = "character");
	idx <- 0;
	if (outcome == "heart attack"){
	idx <- 11
}	else if (outcome == "heart failure"){
		idx <- 17;
}	else if (outcome == "pneumonia"){
		idx <- 23;	
} else {
	stop("Error in best( ",state,", ",outcome," ): invalid outcome", call. = FALSE);
}
        if (is.na(match(state,file$State))){
	stop("Error in best( ",state,", ",outcome," ): invalid state", call. = FALSE)
}
	## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate
	newfile <- file[which(file$State == state),];           
      newfile[,idx] <- as.numeric(newfile[,idx]);
	return(newfile[which.min(newfile[,idx]),2]);
}