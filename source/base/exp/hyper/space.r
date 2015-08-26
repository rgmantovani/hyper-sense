################################################################################################
################################################################################################

# HYPER SPACE object:

# 	- dimension: number of dimensions
# 	- n.eval: number of individuals that will be evaluated
# 	- gs.values: number of values for Grid Search methods
# 	- hyp.param: hyper parameters names
# 	- hyp.range: hyper parameters range
#	- measures: result measures from optimizationX

################################################################################################
################################################################################################

get.hyper.space = function(classifier, n.eval){

	space = NULL;
	temp = paste("get", tolower(classifier), "space", sep=".");
	space = do.call(temp, list(n.eval));

	return(space);
}

################################################################################################
################################################################################################

# * SVM Hyper-parameter space
#	- cost (float)
#	- gamma (float)

get.svm.space = function(n.eval){

	obj = NULL;
	obj$n.eval = n.eval;
	
	# Hyper-parameter space
	obj$hyp.param = c("cost", "gamma", "data.balancing", "feature.selection");
	
	obj$n.dimension = length(obj$hyp.param);

	min.vec = c(-2, -15, 0, 0);
	max.vec = c(15, 3, 1, 1);
	obj$hyp.range = cbind(min.vec, max.vec);
	
	rownames(obj$hyp.range) = obj$hyp.param;
	colnames(obj$hyp.range) = c("min", "max");

	# Metrics returned
	obj$measures = c(obj$hyp.param, "valid.acc", "test.acc", "train.time", 
		"n.support.vectors", "index", "n.solutions", "generations", "n.evaluations", "cpu.time");

	# Population column names
	obj$pop.names = c(obj$hyp.param, "valid.acc", "test.acc", "train.time", "n.sup.vec");

	return(obj);	
}

################################################################################################
################################################################################################

# -C 	pruning confidence (Valores de intervalo: Reif et al, 2014)
# -M 	minimum number of instances
# -N 	number of folds
# -FS    feature selection. (TRUE, FALSE)
# -O    Do not collapse tree. (TRUE, FALSE)
# -R    Use reduced error pruning (TRUE, FALSE).
# -B    Use binary splits only. (TRUE, FALSE)
# -S    Don't perform subtree raising. (TRUE, FALSE)
# -A    Laplace smoothing for predicted probabilities. (TRUE, FALSE)
# -J    Do not use MDL correction for info gain on numeric attributes. (TRUE, FALSE)

#  * C4.5 Hyper-parameter space
get.c45.space = function(n.eval){

	obj = NULL;
	obj$n.evaluation = n.eval;
	
	# Hyper-parameter space
	obj$hyp.param = c("pruning.confidence", "min.n.instances", "n.folds.error", "not.collapse.tree", 
		"using.reduced.error.pruning", "using.only.binary.split", "not.doing.subtree.raising", 
		"using.laplace.smoth", "not.using.MDL.correction", "data.balancing", "feature.selection");

	obj$n.dimension = length(obj$hyp.param);

	#hyper-parameters range
	min.vec = c(0.001, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0);
	max.vec = c(0.5, 50, 10, 1, 1, 1, 1, 1, 1, 1, 1); #No feature selection
	obj$hyp.range = cbind(min.vec, max.vec);
	
	rownames(obj$hyp.range) = obj$hyp.param;
	colnames(obj$hyp.range) = c("min", "max");

	# Metrics returned
	obj$measures = c(obj$hyp.param, "valid.acc", "test.acc", "train.time", 
		"num.leaves", "tree.size","index", "n.solutions", "generations", "n.evaluations", "cpu.time");

	# Population column names
	obj$pop.names = c(obj$hyp.param, "valid.acc", "test.acc", "train.time",  "num.leaves", "tree.size");

	return (obj);
}

################################################################################################
################################################################################################

get.dnn.space = function(n.eval){

	obj = NULL;	
	return (obj);

}

################################################################################################
################################################################################################
