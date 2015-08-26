################################################################################################
################################################################################################

# TODO: multiobjetivo (modelos simples, alta acuracia, baixo tempo)

# Fitness functions for classifiers

################################################################################################
################################################################################################

individual.filter = function(x, hyper.space){

    n = length(x);

    # Rouding elements - positions 2 to 10   
    # x[3:n] = round(x[3:n]);
    x[3:n] = 0;
  
    # Filtering elements
    for(i in 1:n){
        min = hyper.space$hyp.range[i,1];
        max = hyper.space$hyp.range[i,2];

        if(x[i] > max){
            x[i] = max;
        }

        if(x[i] < min){
            x[i] = min;
        }
    }
    return(x);
}

################################################################################################
################################################################################################


fitness.fn = function(x, temp, hyper.space){

    # coloca os indivíduos dentro do padrão necessário
    x = individual.filter(x, hyper.space);
  
    #perform feature selection
    n = length(x);
  
    # if(x[n] == 1){
    #     # cat(' - performing feature selection ...\n');
    #     temp = feature.selection(temp);
    # }
   
	#FOLS-CV extracting the classifier performance
    aux = do.call("rbind", lapply(1:FOLDS, function(i){

        #treinar svm com Train, e validar com teste
        data.train = temp$train[[i]];

        # #Applying data balancing
        # if(x[n-1]){
        #     # cat(" - balancing training data ... \n");
        #     data.train = SMOTE(as.formula("Class ~ ."), data.train)
        #     # cat("depois do smote \n")
        # }

        data.valid = temp$valid[[i]];
        data.test = temp$test[[i]];

		CLASSIFIER = paste("cl", tolower(ALGORITHM), sep=".");
        obj = do.call(CLASSIFIER, list(data.train, data.valid, data.test, x));
        
        valid.acc  = acc.simple (obj$valid.pred, data.valid$Class);
        test.acc  = acc.simple (obj$test.pred, data.test$Class);

        #returning average (validation acc, testing acc, training time, classifier params)
        return( c(valid.acc, test.acc, obj$train.time, obj$params) );

    })); 

    #Média das acurácias sobre o CV
    aux = colMeans(aux);
    POPULATION <<- rbind(POPULATION, c(x, aux));
    
    # Return the validation accuracy as fitness value
    return(aux[1]);
}

################################################################################################
################################################################################################
