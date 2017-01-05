rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
file <- read.csv('outcome-of-care-measures.csv',colClasses = "character");
	idx <- 0;
	if (outcome == "heart attack"){
	idx <- 11
}	else if (outcome == "heart failure"){
		idx <- 17;
}	else if (outcome == "pneumonia"){
		idx <- 23;	
} else {
	stop("invalid outcome", call. = FALSE);
}
        if (is.na(match(state,file$State))){
	stop("invalid state", call. = FALSE)
}
## Return hospital name in that state with the given rank
## 30-day death rate
	newfile <- file[which(file$State == state),];           
      newfile[,idx] <- as.numeric(newfile[,idx]);
	if (num == "best"){
		return(newfile[which.min(newfile[,idx]),2]);
}	else if(num == "worst"){
		return(newfile[which.max(newfile[,idx]),2]);
}	else {
		return(newfile[order(newfile[,idx])[num],2]);
}
}