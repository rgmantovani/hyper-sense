
##########################################################################################################
##########################################################################################################

# Metodos do FSelector Package
# http://cran.r-project.org/web/packages/FSelector/FSelector.pdf

#     ig  = information.gain(f, data);
#     gr  = gain.ratio(f, data);
#     su  = symmetrical.uncertainty(f, data);
#     chi = chi.squared(f, data);
#     lc  = linear.correlation(f, temp)
# [@] rc  = rank.correlation(f, temp)
#     rfi = random.forest.importance(f, data)
#     rl  = relief(f, data)
#     csf =  FSelector::cfs(f, data)
#     cns = consistency(f, data)

##########################################################################################################
##########################################################################################################

#Feature Selection

feature.selection = function(folds){

  f = as.formula("Class ~ .");
  temp = folds;

  # data = rbind(temp$train[[1]], temp$test[[1]], temp$valid[[1]]);
  data = folds$train[[1]];
  n = ncol(data);

  for(i in 1:n){
    data[,i] = as.numeric(as.character(data[,i]));
  }

  rc = FSelector::rank.correlation(f, data)
  rc.exit = FSelector::cutoff.k.percent(attr=rc, k=0.5)
  subset = c(rc.exit, "Class");

  for(i in 1: FOLDS){
      temp$train[[i]] = (folds$train[[i]])[subset];
      temp$valid[[i]] = (folds$valid[[i]])[subset];
      temp$test[[i]]  = (folds$test[[i]])[subset];
  }
  
  return(temp);
}

# feature.selection = function(data, k=7){

#   att = attrEval(Class~., data, estimator="Accuracy");
#   att = sort(att, decreasing=TRUE)[1:k];
#   cat("\t\t - Features Selected: ", names(att), "\n");
#   newbase = cbind(data[names(att)], data$Class);
#   colnames(newbase)[ncol(newbase)] = "Class";
#   return(newbase);
# }


##########################################################################################################
##########################################################################################################