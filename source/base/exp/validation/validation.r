################################################################################################
################################################################################################
# R Code
# k-fold cross validation process
# Metodology for validation

################################################################################################
################################################################################################
holdout = function(data, p){

    tmp=NULL;
    index = sample(1:nrow(data), as.integer(nrow(data)*p),replace=FALSE);
    tmp$train = data[index,];   
    tmp$test = data[-index,]; 

    return(tmp);
}


################################################################################################
################################################################################################

# LOO - leave one out
oneout = function(data) {

    # id = kfold(data, k=nrow(data));
    id = createFolds(data$Class, k=nrow(data), list=TRUE);

    train = lapply(1:nrow(data), function(i) {
        subset(data, id %in% setdiff(1:nrow(data), i));
    });

    test = lapply(1:nrow(data), function(i) {
        subset(data, id %in% i);
    });

    tmp = list();
    tmp$train = train;
    tmp$test = test;
    return(tmp);
}


################################################################################################
################################################################################################
# c-fold stratified

# FOLDS-CV

cfold = function(data,  fold=FOLDS) {

    lines = createFolds(data$Class, k=fold, list=TRUE);

    test = lapply(1:fold, function (i){
        data[ lines[[i]], ]
    });

    train = lapply(1:fold, function(i){
        data[ -lines[[i]], ]
    });

    tmp = list();
    tmp$train = train;
    tmp$test = test;
    return(tmp);
}

################################################################################################
################################################################################################
#c-fold stratified with training/valiation/test folds

cfold.valid = function(data, fold=FOLDS){

    id = createFolds(data$Class, k=fold, list=FALSE);

    #training folds
    train = lapply(1:fold, function(i) {
        if(i == fold){
            subset(data, id %in% setdiff(1:fold, c(i, 1)));
        }
        else{
            subset(data, id %in% setdiff(1:fold, c(i, i+1)));
        }
   });

    #validation folds
    valid = lapply(1:fold, function(i){
        if(i == fold){
            subset(data, id %in% 1)
        }
        else{
            subset(data, id %in% (i+1))
        }
    });

    #testing folds
    test = lapply(1:fold, function(i) {
        subset(data, id %in% i);
    });

    tmp = list();
    tmp$train = train;
    tmp$valid = valid;
    tmp$test = test;
    return(tmp);

}

################################################################################################
################################################################################################