
################################################################################################
################################################################################################

sampling = function(data){

	available = 1:nrow(data);
	used = NULL;
	INDEXES = list();
	values = 1:(length(SCHEDULE)-1)
	
	for(i in values){
	
		if( i == (length(SCHEDULE)-1)){
			samp = 1:nrow(data);
		}else{
			s = round(((SCHEDULE[i+1] - SCHEDULE[i]) * nrow(data))/ 100)
			samp = sample(available, size = s);
		}
		
		used = c(used, samp);
		INDEXES[[i]] = unique(used);
		available = setdiff(available, used);
	
	}
	return(INDEXES);
}


################################################################################################
################################################################################################

# print.sampling = function(available, used, s){

# 	cat("\n[*] Schedule: ", s, " per cent \n");
# 	cat("[*] Available indexes: \n");
# 	print(available);
# 	cat("[*] Used indexes: \n");
# 	print(used);

# }

################################################################################################
################################################################################################