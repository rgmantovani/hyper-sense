################################################################################################
################################################################################################

creating.folders =  function(){

	# Creating trace dir - populations stored
	dirs = NULL;
	trace.dir = paste(DIR, "/trace/", tolower(ALGORITHM), sep="")
	if(!file.exists(trace.dir)){
		cat(" - Creating trace dir (storing complete trace from heuristics - populations). \n");
		dir.create(trace.dir, recursive=TRUE);
	}

	# Creating folds dir - folds partitions
	folds.dir = paste(DIR, "/folds", sep="");
	if(!file.exists(folds.dir)){
		cat(" - Creating folds dir (storing dataset partitions). \n");
		dir.create(folds.dir);
	}

	# Creating output dir (in database level)
	out.dir = paste(DIR, OUTPUT, tolower(ALGORITHM), sep="")
	if(!file.exists(out.dir)){
		cat(" - Creating output dir. (storing results) \n");
		dir.create(out.dir, recursive=TRUE);
	}

	# Creating temp dir (in base level)
	temp.dir = paste(DIR, "/temp/", tolower(ALGORITHM), sep="")
	if(!file.exists(temp.dir)){
		cat(" - Creating temp dir (storing uncomplete results). \n");
		dir.create(temp.dir, recursive=TRUE);
	}

	dirs$temp.dir = temp.dir;
	dirs$trace.dir = trace.dir;
	dirs$folds.dir = folds.dir;
	dirs$out.dir = out.dir;

	return(dirs);
}

################################################################################################
################################################################################################