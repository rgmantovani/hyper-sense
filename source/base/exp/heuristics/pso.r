################################################################################################
################################################################################################

# PSO: Particle Swarm Optimization

################################################################################################
################################################################################################
pso.svm = function(folds, hyper.space){

	n = hyper.space$n.dimension;
	dimensions = hyper.space$hyp.range;
	POPULATION <<- c();

	# Calling PSO from package pso
	# par = DF.SVM - sugestao de particula inicial (default da SVM)
	t = system.time(
		obj <- psoptim(par=DF.SVM, fn=fitness.fn, temp=folds, 
			hyper.space=hyper.space, lower=dimensions[,1],upper=dimensions[,2],
			control=list(fnscale=-1, s=POP.SIZE, trace=0, trace.stats=FALSE, 
			maxit=MAX.ITERATIONS)
		)
	);

	# Best solution
	best = round(obj$par,ROUNDING);
	# best[3:n] = round(best[3:n]);
	best[3:n] = 0;


	# Rounding Population
	POPULATION = round(POPULATION, ROUNDING);
	subpop = POPULATION[, 1:n];

	# Number of Good solutions
	valid.fit = round((-1* obj$value), ROUNDING);
	n.solutions = length(which(POPULATION[,n+1] == valid.fit));

	#Getting best individual measures values
	ret = which(apply(subpop, 1, function(x) all(x %in% best)))
	index = ret[1];

	#Returning the best solutions
	mth = NULL;
	mth$measures = c(POPULATION[index, ], index, n.solutions,
	 	obj$counts[2], obj$counts[1], t[3]);
	names(mth$measures) = hyper.space$measures;

	mth$population = POPULATION;
	colnames(mth$population) = hyper.space$pop.names;

	return(mth);
}

################################################################################################
################################################################################################
