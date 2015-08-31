################################################################################################
################################################################################################

creating.folders =  function(datafile){

	# Creating trace dir - populations stored
	trace.dir = paste(DIR, "/trace/", tolower(ALGORITHM), "/", datafile, sep="")
	if(!file.exists(trace.dir)){
		cat(" - Creating trace dir (storing complete trace from heuristics - populations). \n");
		dir.create(trace.dir, recursive=TRUE);
	}

	# Creating folds dir - folds partitions
	folds.dir = paste(DIR, "/folds/", datafile, sep="");
	if(!file.exists(folds.dir)){
		cat(" - Creating folds dir (storing dataset partitions). \n");
		dir.create(folds.dir, recursive=TRUE);
	}

	# Creating output dir (in database level)
	out.dir = paste(DIR, OUTPUT, tolower(ALGORITHM), "/" , datafile, sep="")
	if(!file.exists(out.dir)){
		cat(" - Creating output dir. (storing results) \n");
		dir.create(out.dir, recursive=TRUE);
	}

	dirs = NULL;
	dirs$trace.dir = trace.dir;
	dirs$folds.dir = folds.dir;
	dirs$out.dir = out.dir;

	return(dirs);
}

################################################################################################
################################################################################################