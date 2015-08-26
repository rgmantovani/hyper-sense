################################################################################################
################################################################################################

root = function(files, dirs, hyper.space) {
	
	count = 0;
	
	# For each dataset file
	for(file in files){
		
		filename =  gsub("*\\.(\\w*)", "\\", file);
		count = count + 1;

		# output file - current dataset
		meta.file = paste(dirs$out.dir, "/", filename, ".RData", sep="");
		trace.file = paste(dirs$trace.dir, "/", "trace-", filename, ".RData", sep="");

		cat("[#] File:", count ,"/",length(files),"-",file,"\n");

		if(! file.exists(meta.file) ){

			#temp folder to these dataset
			temp.folder = paste(dirs$temp.dir, "/", filename, "/" , sep="")
			if(! file.exists(temp.folder)){
				dir.create( temp.folder , recursive=TRUE);
			}
			dirs$temp.dir = temp.folder;

			# Reading dataset
			data = read.arff(paste(HOMEDIR, DATABASE, SUBDIR, file, sep=""));

			# Running EPOCHS times
			cat("/")
			heur = parallel::mclapply(1:EPOCHS,  function(epoch){
				obj = pattern(data, filename, dirs, hyper.space, epoch);
				cat("=");
				return (obj);
			}, mc.cores = parallel::detectCores());
			cat("/\n");

			#Remove the dataset temporary folder and files
			unlink(dirs$temp.dir, recursive = TRUE);

			
			# Binding the Full data frame with results
			full = do.call("rbind", lapply(1:EPOCHS, function(i){
				exec = heur[[i]];
				temp = do.call("rbind", lapply(exec, function(schedule){
					return(c(schedule$schedule, schedule$measures));
				}));
				return(cbind(i, temp));
			}));
			colnames(full)[1] = "Execution";
 			colnames(full)[2] = "Schedule";

			# Saving measures from all executions
			dump("full", meta.file)


			# Saving complete trace of all executions
			traces = do.call("rbind", lapply(1:EPOCHS, function(i){
				exec = heur[[i]];
				temp = do.call("rbind", lapply(exec, function(schedule){
					return(c(schedule$schedule, schedule$measures));
				}));
				return(cbind(i, temp));
			}));

			all.traces = heur;
			dump("all.traces", trace.file);
		
		}else{
			cat (" ... already computed ... \n");
		}
	}
}

################################################################################################
################################################################################################
pattern = function(data, filename, dirs, hyper.space, i){

	# returning object
	results.file = paste(dirs$temp.dir, "results-", i, ".RData", sep="");

	#Verify if results are already done for this thread
	if(!file.exists(results.file)){

		#Reading/Creating folds from data sampling
		path = paste(dirs$folds.dir,"/", filename, sep="");
		if(!file.exists(path)){
			dir.create(path)
		}
	
		#Run for the schedule
		ret = run.schedule(data, path, i, results.file);
  	}
  	#Getting results alredady computed from files on disk
  	else{
  		data = dget(results.file);
  		ret = data;
  	}

	return(ret);
}

################################################################################################
################################################################################################

run.schedule = function(data, path, i, results.file){

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
		
		dumped.file = paste(path, "/schedule-", s, "-fold-", i, ".RData", sep="");
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

	#dumping object
	dump("aux", file = results.file);

	return(aux);
}

################################################################################################
################################################################################################