################################################################################################
################################################################################################

# -C <pruning confidence>
#         Set confidence threshold for pruning.  (default 0.25)
# 	Number of arguments: 1.

# -M <minimum number of instances>
#         Set minimum number of instances per leaf.  (default 2)
# 	Number of arguments: 1.

# -N <number of folds>
#         Set number of folds for reduced error pruning. One fold is used as pruning set.  (default 3)
# 	Number of arguments: 1.

# -U      Use unpruned tree. (TRUE, FALSE)
# -O      Do not collapse tree. (TRUE, FALSE)
# -R      Use reduced error pruning (TRUE, FALSE).
# -B      Use binary splits only. (TRUE, FALSE)
# -S      Don't perform subtree raising. (TRUE, FALSE)
# -A      Laplace smoothing for predicted probabilities. (TRUE, FALSE)
# -J      Do not use MDL correction for info gain on numeric attributes. (TRUE, FALSE)

################################################################################################
################################################################################################
# Chamda Default do Rweka:
# C = 0.25, M = 2, N = 3, 
# U = FALSE, O = TRUE, R = FALSE, B = FALSE, S = TRUE, A = FALSE, J = TURE

# cl.c45 = function(train, valid, test, params=DF.C45)
cl.c45 = function(train, valid, test, params=DF.C45)
{
	#defining object of control
	c45.control = c45.validate.individual(params);
	
	# Inducing the model ...
	t = system.time( model <- RWeka::J48(Class ~ ., train, control = c45.control));
	
	# Predicting ...
	pred = predict(model, test[,-ncol(test)], type="class");
	names(pred) = row.names(test);
	
	obj = NULL;
	obj$train.time = t[3];
	obj$valid.pred = predict(model, valid[,-ncol(valid)]);
	obj$test.pred  = predict(model, test[,-ncol(test)]);

	num.leaves = model$classifier$measureNumLeaves();
    tree.size = model$classifier$measureTreeSize();

	obj$params = c(num.leaves, tree.size);

	return(obj);
}


################################################################################################
################################################################################################

#Conditions:

c45.validate.individual = function(params){

	my.control = NULL;
	names(params) = c("C", "M", "N", "O", "R", "B", "S", "A", "J", "DB", "FS");

	# Pruned Tree options
	# OBS: Is the tree is Pruned we can set N and C hyper-parameter if R is true
	if(params["R"] == 1){
		my.control = RWeka::Weka_control(M = params["M"], N = params["N"],
			O = (params["O"] == 1), R = (params["R"] == 1),	B = (params["B"] == 1), 
			S = (params["S"] == 1), A = (params["A"] == 1), J = (params["J"] == 1));

	}else{
		my.control = RWeka::Weka_control(C = params["C"], M = params["M"],	
			O = (params["O"] == 1), R = (params["R"] == 1),	B = (params["B"] == 1), 
			S = (params["S"] == 1), A = (params["A"] == 1), J = (params["J"] == 1));
	}
	return(my.control);
}

################################################################################################
################################################################################################
