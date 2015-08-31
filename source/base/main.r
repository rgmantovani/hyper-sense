################################################################################################
################################################################################################

#Loading all R Files
setup = function() {

	#loading files in the current task subir
	files = list.files(path="exp", pattern="\\.r$", recursive=TRUE, full.name=TRUE);	
	for(i in files){
		source(i);
	}
}

################################################################################################
################################################################################################

run = function(datafile, epoch){

	# * Getting Hyper-space problem
	classifier = ALGORITHM;
	n.eval = (POP.SIZE * MAX.ITERATIONS);
	hyper.space = get.hyper.space(classifier, n.eval);

	#Reading Dataset Files
	file = paste(HOMEDIR, DATABASE, SUBDIR, datafile, ".arff", sep="");
	
	if(file.exists(file)){

		#Running ML algorithms and Optimizatin techniques
		dirs = creating.folders(datafile);
		root(datafile, dirs, hyper.space, epoch);
		# cat("\n - Done ... \n");
		
	}else{
		cat(" ERROR: The database file/path is not correct. Please, 
			verify the \'subdir\' parameter in the config file.\n");
	}

}

################################################################################################
################################################################################################

main = function(datafile, epoch){

	cat(" * Loading R files ... \n\n");
	setup();
	
	cat(" * Running Base level algorithms ... \n\n");
	cat(" [*] Execution number: ", epoch ,"\n");
	run(datafile, epoch);

}

################################################################################################
################################################################################################

options(echo=TRUE) 
args <- commandArgs(trailingOnly = TRUE)

# Parse arguments (we expect the form --arg=value)
parseArgs <- function(x) strsplit(sub("^--", "", x), "=")
argsDF <- as.data.frame(do.call("rbind", parseArgs(args)))
argsL <- as.list(as.character(argsDF$V2))

datafile = argsL[[1]];
epoch = as.numeric(argsL[[2]]);

main(datafile, epoch);

################################################################################################
################################################################################################
