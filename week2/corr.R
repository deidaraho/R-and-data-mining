corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
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
	k = 0;
	corr_out = numeric(); 
	for (i in 1:332) {
		s_temp = myMatrices[[i]][2];
		n_temp = myMatrices[[i]][3];
		sum_temp = sum((!is.na(s_temp))*(!is.na(n_temp)));
		if (sum_temp > threshold) {
			k = k+1;
			corr_out = append(corr_out,cor(s_temp,n_temp,use = "complete"));
}
}
	return(corr_out);
}