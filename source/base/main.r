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

run = function() {

	# * Getting Hyper-space problem
	classifier = ALGORITHM;
	n.eval = (POP.SIZE * MAX.ITERATIONS);
	hyper.space = get.hyper.space(classifier, n.eval);

	#Reading Datasets Files
	files = list.files(paste(HOMEDIR, DATABASE, SUBDIR, sep=""));
	# files = files[21];
	if(length(files) != 0){

		dirs = creating.folders();
		#Running ML algorithms and Optimizatin techniques
		root(files, dirs, hyper.space);
		
		cat("\n - Done ... \n");
		
	}else{
		cat(" ERROR: The database directory is not correct. Please, 
			verify the \'subdir\' parameter in the config file.\n");
	}

}

################################################################################################
################################################################################################

main = function(){

	cat(" * Loading R files ... \n\n");
	setup();
	
	cat(" * Running Base level algorithms ... \n\n");
	run();

}

################################################################################################
################################################################################################

main();

################################################################################################
################################################################################################
