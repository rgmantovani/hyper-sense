################################################################################################
################################################################################################

balancing.data = function(data){

	n.tun = length(which(data[,ncol(data)] == "tun"));
	n.df = length(which(data[,ncol(data)] == "df"));
	# prop = round((n.df/n.tun)*100);
	prop = ((n.tun / n.df) - 1) * 100;

	#Balanceando classes
	temp = SMOTE(Class ~ ., data, perc.over=prop, perc.under=0);
	idx = which(data[,ncol(data)] == "tun");
	newdata = rbind(data[idx, ], temp)

	return(newdata);
}

################################################################################################
################################################################################################