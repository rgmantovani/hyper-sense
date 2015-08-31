################################################################################################
################################################################################################

root = function(datafile, dirs, hyper.space, epoch) {
	
	# output file - current dataset
	meta.file = paste(dirs$out.dir, "/", datafile, "-", epoch, ".RData", sep="");
	trace.file = paste(dirs$trace.dir, "/", "trace-", datafile, "-", epoch, ".RData", sep="");

	cat("[#] File:", datafile,"\n");

	if(! file.exists(meta.file) ){

		# Reading dataset
		cat (" - reading dataset ... \n");
		data = read.arff(paste(HOMEDIR, DATABASE, SUBDIR, datafile, ".arff", sep=""));
		cat (" - calling optimization techniques ... \n ");
		obj = pattern(data, dirs, hyper.space, epoch);
		cat (" - results generated ... \n");

		# Binding the Full data frame with results
		full = do.call("rbind", lapply(obj, function(sch){
			return(c(sch$schedule, sch$measures));
		}));
	 	colnames(full)[1] = "Sampling";

		# Saving measures and trace from all executions
		dump("full", meta.file)
		dump("obj", trace.file);
		cat(" - results saved in files ...\n");
			
	}else{
		cat (" ... already computed ... \n");
	}
	
}

################################################################################################
################################################################################################
pattern = function(data, dirs, hyper.space, epoch){

	#Get incremental indexes
	INDEXES = sampling(data);

	# Default values from Libsvm
	params = c(1, (1/(ncol(data)-1)), 0, 0);
	DF.SVM <<- params;

	# Running for all Schedules
	aux = lapply(2:length(SCHEDULE), function(k){

		s = SCHEDULE[k];
		indexes = INDEXES[[k-1]];
		temp = data[indexes, ];
		
		dumped.file = paste(dirs$folds.dir, "/sampling-", s, "-fold-", epoch, ".RData", sep="");
		# 8 - 1 - 1 division | load previous division
		if(!file.exists(dumped.file)){
			folds = cfold.valid(temp);
			dump("folds", dumped.file);
		}else{
			folds = dget(dumped.file);
		}

		# Techniques to be called ...
		TECHNIQUES = paste(tolower(HEURISTICS), tolower(ALGORITHM), sep=".")

		#executing meta-heuristics techniques
		heur = lapply(TECHNIQUES, function(mth) {
			tmp = do.call(mth, list(folds, hyper.space));
			return(tmp);
		});

		ret = NULL;
		ret$schedule = s;
		ret$measures = unlist(lapply(heur, function(temp) temp$measures));
		names(ret$measures) = paste(rep(HEURISTICS, each=length(hyper.space$measures)), 
	  		rep(hyper.space$measures, times = length(HEURISTICS)), sep=".");

		# #populations from all meta-heuristics
		ret$trace = lapply(heur, function(temp) temp$population);
		return(ret);	
	});

	return(aux);
}

################################################################################################
################################################################################################