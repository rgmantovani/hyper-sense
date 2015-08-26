################################################################################################
################################################################################################

# Classifiers Call: 

#  * [parameters]:
# 	- params = c(cost, gamma) - hyper-parameters
# 	- train: examples from training folds
# 	- test: examples from test fold
# 	- valid: examples from validation fold

#  * [return]:
#	- obj$model: the induced model
#	- obj$valid.pred: the predictions using the validation fold
# 	- obj$test.pred: the predictions using the test fold 
# 	- obj$train.time: the training time spent to induce the model

#SVM training model - 8/1/1 data division

################################################################################################
################################################################################################

# Hyper-parameters:
# 	- Cost: 
#	- gamma: 

# cl.svm = function(train, valid, test, params=c(1, 0.01, 0)){
	
# 	obj = NULL;
# 	params = 2^params;
	
# 	t = system.time(model <- svm(Class ~ ., train, kernel="radial", 
# 		cost = params[1], gamma=params[2]));
	
# 	obj$train.time = t[3];
# 	obj$valid.pred = predict(model, valid[,-ncol(valid)]);
# 	obj$test.pred  = predict(model, test[,-ncol(test)]);
# 	obj$params = model$tot.nSV;

# 	return(obj);
# }

cl.svm = function(train, valid, test, params=DF.SVM){
	
	#Se não for default não precisa elevar a base 2	
	if(!all(params == DF.SVM)){
		params = 2^params;
	}

	t = system.time(model <- svm(Class ~ ., train, kernel="radial", 
		cost = params[1], gamma=params[2]));
	
	obj = NULL;
	obj$train.time = t[3];
	obj$valid.pred = predict(model, valid[,-ncol(valid)]);
	obj$test.pred  = predict(model, test[,-ncol(test)]);
	obj$params = model$tot.nSV;

	return(obj);
}

################################################################################################
################################################################################################