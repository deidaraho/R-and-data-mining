complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
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
	id_num = length(id);
	id_out = vector(mode='numeric',id_num);
	np_out = vector(mode='numeric',id_num);
	k = 1
	for (i in id) {
	id_out[k] = i;
	s_temp = myMatrices[[i]][2];
	n_temp = myMatrices[[i]][3];
	sum_temp = sum((!is.na(s_temp))*(!is.na(n_temp)));
	np_out[k] = sum_temp;
	k = k+1
}
	return(data.frame(id=id_out,nobs=np_out))
}