################################################################################################
################################################################################################

df.svm = function(folds, hyper.space){

	n = hyper.space$n.dimension;
	POPULATION <<- c();

	t = system.time( best <- fitness.fn(DF.SVM, folds, hyper.space));
  
   	#Returning values
	mth = NULL;
	mth$measures = c(POPULATION[1, ], 1, 1, 1, 1, t[3]);
	mth$measures = round(mth$measures, ROUNDING);
	mth$population = round(POPULATION, ROUNDING);

	names(mth$measures) = hyper.space$measures;
	colnames(mth$population) = hyper.space$pop.names;

	return(mth);
}

################################################################################################
################################################################################################
