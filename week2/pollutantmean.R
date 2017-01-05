pollutantmean <- function(directory, pollutant, id = 1:332) {
      ## 'directory' is a character vector of length 1 indicating
      ## the location of the CSV files

      ## 'pollutant' is a character vector of length 1 indicating
      ## the name of the pollutant for which we will calculate the
      ## mean; either "sulfate" or "nitrate".

      ## 'id' is an integer vector indicating the monitor ID numbers
      ## to be used

      ## Return the mean of the pollutant across all monitors list
      ## in the 'id' vector (ignoring NA values)
 	# Get the list of files
  	#----------------------------#
    	folder <- paste('./',directory,sep='');
    	fileList <- dir(folder, recursive=TRUE);  
	# grep through these, if you are not loading them all
    	# use platform appropriate separator
    	files <- paste(folder, fileList, sep=.Platform$file.sep);
  	# Read them in
  	#----------------------------#
    	myMatrices <- lapply(files, read.csv);	
	# Check the index number
	indx = 2;				
	if (pollutant == 'nitrate'){
	indx = 3;
	}
	num_tol = 0;
	sum_tol = 0;
	for (i in id) {
	c_temp = myMatrices[[i]][indx];
	num_temp = sum(!is.na(c_temp));
	sum_temp = 0 
	if (num_temp>0){
	sum_temp = sum(c_temp,na.rm=TRUE);
	}	
	sum_tol = sum_tol + sum_temp;
	num_tol = num_tol + num_temp;
	}
	return(sum_tol/num_tol)
}