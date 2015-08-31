##########################################################################################################
# Ploting Graphics
##########################################################################################################

require("reshape2");
require("ggplot2");

##########################################################################################################
##########################################################################################################

heatmap.plot = function(dataset){

 	pso.data = dataset[,1:15];
 	rs.data = dataset[,c(1,2,16:28)];
	df.data = dataset[,c(1,2,29:41)];

	#df value
	df.value = df.data[1,3:4];

	heatmap.plot.aux(df.value, pso.data, "PSO");
	heatmap.plot.aux(df.value, rs.data, "RS");
	
}

##########################################################################################################
##########################################################################################################


heatmap.plot.aux = function(df.value, data, alg){

	data[,3:4] = round(data[,3:4],3);
	df = as.data.frame(data);
	df = melt(df, id.vars=-8);
	colnames(df)[3] = "cost";
	colnames(df)[4] = "gamma";

	filename = paste(alg, "-heatmap.pdf",sep="")
	pdf(filename, height=5);

	g = NULL;
	g = ggplot(df, aes(x=cost, y=gamma, colour=value))#, size=fitness)); 

	g = g + scale_y_continuous(limits=c(-15, 3));
	g = g + scale_x_continuous(limits=c(-2, 15));
	g = g + scale_colour_gradient(low="yellow", high="red"); #(low="blue", high="red");
	g = g + geom_point(aes(shape=as.factor(15))); 
	# g = g + ggtitle(paste(alg,"Hyper-parameters space",sep=" "));

	#Default
	sp = data.frame(cost=df.value[1], gamma=df.value[2])
	g = g + geom_point(data=sp,aes(x=cost,y=gamma),colour="black",size=4)

	
	# g = g + ggtitle("Espaço de Hiper-parâmetros para SVMs");
	g = g + ggtitle("SVM hyper-parameters space");
	
	# g = g + scale_size(guide = 'none')
	g = g + scale_shape(guide = 'none')
	
	print(g);
	dev.off();
}


# improvement.plot = function(data){

# 	# 1 - 23
# 	filename = "improvement-1.pdf";
# 	pdf(filename, height=4.5, width=10);

# 	df1 = melt(data[1:23,]);
# 	colnames(df1)[2] = "technique"
# 	g = ggplot(df1, aes(x=dataset, y=value, fill=technique, group=technique, width=.75))
#    	g = g + geom_bar(position="dodge",stat="identity")
   	
#    	mP = c("#000000", "#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")
# 	g = g + scale_fill_manual(values=mP)
# 	g = g + ylab("Test Accuracy") + xlab("Data set");
# 	g = g + theme(axis.text.x = element_text(angle = 40, hjust = 1))
# 	print(g);
# 	dev.off();

# 	# 24 - 47
# 	filename = "improvement-2.pdf";
# 	pdf(filename, height=4.5, width=10);

# 	df2 = melt(data[24:47,]);
# 	colnames(df2)[2] = "technique"
# 	g = ggplot(df2, aes(x=dataset, y=value, fill=technique, group=technique, width=.75))
#    	g = g + geom_bar(position="dodge",stat="identity")
   	
#    	mP = c("#000000", "#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")
# 	g = g + scale_fill_manual(values=mP)
# 	g = g + ylab("Test Accuracy") + xlab("Data set");
# 	g = g + theme(axis.text.x = element_text(angle = 40, hjust = 1))
# 	print(g);
# 	dev.off();

# 	# 48 - 70
# 	filename = "improvement-3.pdf";
# 	pdf(filename, height=4.5, width=10);

# 	df3 = melt(data[48:70,]);
# 	colnames(df3)[2] = "technique"
# 	g = ggplot(df3, aes(x=dataset, y=value, fill=technique, group=technique, width=.75))
#    	g = g + geom_bar(position="dodge",stat="identity")
   	
#    	mP = c("#000000", "#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")
# 	g = g + scale_fill_manual(values=mP)
# 	g = g + ylab("Test Accuracy") + xlab("Data set");
# 	g = g + theme(axis.text.x = element_text(angle = 40, hjust = 1))
# 	print(g);
# 	dev.off();

# }


##########################################################################################################
##########################################################################################################

#Plot das geracoes em que foram encontrados os melhores individuos

# generations.plot = function(data){

# 	df = melt(data, id.vars=c("max", "min", "mean"));
	
# 	limits = aes(ymax = max, ymin=min)
# 	dodge = position_dodge(width=0.9)

# 	filename = "bests-generations.pdf";
# 	pdf(filename, height=4.5, width=10);

# 	g = ggplot(df, aes(x=value, y=mean)) # width=.75))
# 	g = g + geom_bar( stat="identity", fill="#999999", colour="black", position=dodge)
# 	g = g + geom_text(aes(label=mean), size=3, vjust=-0.7, hjust=1.2)
# 	g = g + guides(fill=FALSE)
# 	g = g + ylab("Generation") + xlab("Data set");
# 	g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 	g = g + geom_errorbar(limits, position=dodge, width=0.25) # linetype="dotted")
# 	g = g + geom_hline(yintercept=25, linetype="dotted", colour="red")

# 	print(g);
# 	dev.off();


# }

##########################################################################################################
##########################################################################################################


#plots alterantivos - primeiras e ultimas execucoes das técnicas

heatmap.alterantive.first.last = function(data){
	
	#df values = 
	alldf = do.call("rbind", do.call("rbind", (lapply(data$df, function(tmp) tmp))));
	df.value = alldf[1,1:2];

	#primeiras e ultimas geracoes de cada execucao, de cada fold
	# - GA
	firsts.ga = lapply(data$ga, function(tmp) lapply(tmp, function(aux) aux[1:10,]));
	lasts.ga = lapply(data$ga, function(tmp) lapply(tmp, function(aux) aux[(nrow(aux)-9):nrow(aux),]));

	allga.firsts = do.call("rbind", do.call("rbind",firsts.ga));
	allga.lasts = do.call("rbind", do.call("rbind",lasts.ga));
	 
	# heatmap.plot(allga.firsts, "GA-firsts");
	heatmap.plot(df.value, allga.lasts, "GA-lasts");

	# - PSO
	firsts.pso = lapply(data$pso, function(tmp) lapply(tmp, function(aux) aux[1:10,]));
	lasts.pso = lapply(data$pso, function(tmp) lapply(tmp, function(aux) aux[(nrow(aux)-9):nrow(aux),]));

	allpso.firsts = do.call("rbind", do.call("rbind",firsts.pso));
	allpso.lasts = do.call("rbind", do.call("rbind",lasts.pso));
	 
	# heatmap.plot(allpso.firsts, "PSO-firsts");
	heatmap.plot(df.value, allpso.lasts, "PSO-lasts");


	# - EDA
	firsts.eda = lapply(data$eda, function(tmp) lapply(tmp, function(aux) aux[1:10,]));
	lasts.eda = lapply(data$eda, function(tmp) lapply(tmp, function(aux) aux[(nrow(aux)-9):nrow(aux),]));

	alleda.firsts = do.call("rbind", do.call("rbind",firsts.eda));
	alleda.lasts = do.call("rbind", do.call("rbind",lasts.eda));
	 
	# heatmap.plot(alleda.firsts, "EDA-firsts");
	heatmap.plot(df.value, alleda.lasts, "EDA-lasts");

	# - RS
	firsts.rs = lapply(data$rs, function(tmp) lapply(tmp, function(aux) aux[1:10,]));
	lasts.rs = lapply(data$rs, function(tmp) lapply(tmp, function(aux) aux[(nrow(aux)-9):nrow(aux),]));

	allrs.firsts = do.call("rbind", do.call("rbind",firsts.rs));
	allrs.lasts = do.call("rbind", do.call("rbind",lasts.rs));
	 
	# heatmap.plot(allrs.firsts, "RS-firsts");
	heatmap.plot(df.value, allrs.lasts, "RS-lasts");

}

##########################################################################################################
##########################################################################################################

main.plots = function(data){

	#parametros para plot do heat map
	allga = do.call("rbind", do.call("rbind", (lapply(data$ga, function(tmp) tmp))));
	allpso = do.call("rbind", do.call("rbind", (lapply(data$pso, function(tmp) tmp))));
	alleda = do.call("rbind", do.call("rbind", (lapply(data$eda, function(tmp) tmp))));
	allrs= do.call("rbind", do.call("rbind", (lapply(data$rs, function(tmp) tmp))));
	allgs = do.call("rbind", do.call("rbind", (lapply(data$gs, function(tmp) tmp))));
	alldf = do.call("rbind", do.call("rbind", (lapply(data$df, function(tmp) tmp))));

	all = list()
	all[[1]] = data$ga;
	all[[2]] = data$pso;
	all[[3]] = data$eda;
	all[[4]] = data$rs;
	all[[5]] = data$gs;
	all[[6]] = data$df;
	
	#fintess para plot das linhas
	# all.fitness.plot(data$ga, "GA");
	# fold.fitness.plot(data$ga, "GA"); 
	mth.fitness.plot(all);

	#heatMap	
	heatmap.plot(allga, "GA");
	heatmap.plot(allpso, "PSO");
	heatmap.plot(alleda, "EDA");
	heatmap.plot(allrs, "RS");
	heatmap.plot(allgs, "GS");
	heatmap.plot(alldf, "DF");

}

##########################################################################################################
##########################################################################################################

get.data = function(data){
	
	#garantir que tenha pelo menos 50 elementos no dataset para plot
	times = 50 - nrow(data);

	if(is.null(nrow(data))){
		first = data;
		times = 49;
	}
	else{
		first = data[1,];
	}

	for(i in 1:times){
		data = rbind(first, data)
	}
	rownames(data) = NULL;

	ret = c();
	for(i in 1:5){
		s = seq(10*(i-1)+1,10*i);
		temp = data[s,3];
		ret = rbind(ret, c(i, mean(temp), sd(temp)))
	}

	colnames(ret) = c("generation", "meanfit", "stdfit");
	return(ret);
}

##########################################################################################################
##########################################################################################################
# all.fitness.plot = function(data, alg){

# 	filename = paste(alg, "-all-fitness.pdf",sep="")
# 	pdf(filename, height=5);
# 	g = NULL;

# 	for(i in 1:length(data)){
# 		for(j in 1:length(data[[1]])){

# 			df = as.data.frame(get.data(data[[i]][[j]]));
# 			df = melt(df, id=colnames(df));
			
# 			if(i == 1 && j == 1){
# 				g = ggplot(data=df,aes(x=generation, y=meanfit))
# 				g = g + geom_line()+ geom_point()+ scale_fill_brewer();
# 			}
# 			else{
#  				g = g + geom_line(data=df) + geom_point(data=df);
# 			}
# 		}
# 	}

# 	g = g + scale_y_continuous(limits = c(0, 1))
# 	g = g + scale_x_continuous(limits = c(1, 5))
# 	g = g + ylab("Fitness Value") + xlab("Generation");
# 	g = g + ggtitle(paste(alg,"fitness evaluation",sep=" "));
#  	print(g);
# 	dev.off();

# }

##########################################################################################################
##########################################################################################################
# fold.fitness.plot = function(data, alg){

# 	filename = paste(alg, "-fold-fitness.pdf",sep="")
# 	pdf(filename, height=5);
# 	g = NULL;

# 	#iterando por fold
# 	for(i in 1:length(data)){

# 		fold.data = c();
# 		#iterando por execucao da heur
# 		for(j in 1:length(data[[1]])){
# 			temp = as.data.frame(get.data(data[[i]][[j]]))
# 			fold.data = rbind(fold.data, c(temp$meanfit, temp$stdfit))
# 		}

# 		#1 ao 5  - meanfit do fold
# 		#6 ao 10 - stdfit do fold
# 		fold.data = colMeans(fold.data);
	
# 		generation=1:5;
# 		meanfit = fold.data[1:5];
# 		df = as.data.frame(cbind(meanfit,generation));
# 		df = melt(df, id=colnames(df));
			
# 		if(i == 1){
# 			g = ggplot(data=df,aes(x=generation, y=meanfit))
# 			g = g + geom_line()+ geom_point()+ scale_fill_brewer();
# 		}
# 		else{
#  			g = g + geom_line(data=df) + geom_point(data=df);
# 		}
# 	}

# 	g = g + scale_y_continuous(limits = c(0, 1))
# 	g = g + scale_x_continuous(limits = c(1, 5))
# 	g = g + ylab("Fitness value") + xlab("Generation");
# 	g = g + ggtitle(paste(alg,"fitness evaluation",sep=" "));
#  	print(g);
# 	dev.off();

# }

# ##########################################################################################################
# ##########################################################################################################
# mth.fitness.plot = function(all){

# 	#iterando por heuristica
# 	final.data = c();
# 	for(k in 1:length(all)){

# 		#iterando por fold
# 		data = all[[k]];

# 		mth.data = c()
# 		for(i in 1:length(data)){

# 			fold.data = c();
# 			#iterando por execucao da heur
# 			for(j in 1:length(data[[1]])){
# 				temp = as.data.frame(get.data(data[[i]][[j]]))
# 				fold.data = rbind(fold.data, temp$meanfit)#, temp$stdfit))
# 			}
# 			fold.data = colMeans(fold.data);
# 			mth.data = rbind(mth.data, fold.data);
# 		}

# 		mth.data = colMeans(mth.data);
# 		#TODO: desvio padrao tb das colunas
# 		final.data = cbind(final.data, mth.data);
# 	}

# 	#colnames(final.data) = c("ga", "pso", "eda", "rs", "gs", "df");
# 	generation=1:5;
	
# 	df = cbind(generation,final.data[,1],"GA");
# 	df = rbind(df, cbind(generation,final.data[,2],"PSO"))
# 	df = rbind(df, cbind(generation,final.data[,3],"EDA"))
# 	df = rbind(df, cbind(generation,final.data[,4],"RS"))
# 	#df = rbind(df, cbind(generation,final.data[,5],"GS"))
# 	#df = rbind(df, cbind(generation,final.data[,6],"DF"))

# 	filename = paste("mth-fitness.pdf",sep="")
# 	pdf(filename, height=5);
# 	g = NULL;

# 	colnames(df) = c("generation", "meanfit", "Algorithm")
# 	# colnames(df) = c("generation", "meanfit", "Tecnica")
	
# 	df = as.data.frame(df);
# 	df = melt(df, id=colnames(df));
# 	df[,1] = as.numeric(as.character(df[,1]));
# 	df[,2] = as.numeric(as.character(df[,2]));

# 	g = ggplot(data=df,aes(x=generation, y=meanfit, colour=Tecnica, linetype=Tecnica));
# 	g = g + geom_line()+ geom_point()+ scale_fill_brewer();

# 	# g = g + scale_y_continuous(limits = c(0, 1))
# 	# g = g + scale_x_continuous(limits = c(1, 5))

# 	# g = g + ylab("Fitness value") + xlab("Generation");
# 	# g = g + ggtitle(paste("Mth fitness evaluation",sep=" "));

# 	g = g + ylab("Valor de Fitness") + xlab("Geração");
# 	g = g + ggtitle(paste("Valores médios de fitness para as técnicas de PT",sep=" "));

#  	print(g);
# 	dev.off();

# }

##########################################################################################################
##########################################################################################################

# Histograma da acuracia dos individuos

# ind.histogram = function(data, dataname){

# 	all = do.call("rbind", do.call("rbind", data));

# 	filename = paste(dataname, "acc-distribution.pdf",sep="")
# 	pdf(filename, height=5);
# 	m = NULL;

# 	df.test = melt(all[,4])
# 	df.val = melt(all[,3])	

# 	df.test$accuracy = "test";
# 	df.val$accuracy = "validation";

# 	vegLengths = rbind(df.test, df.val);

# 	m = ggplot(vegLengths, aes(x=value, fill = accuracy)) + geom_histogram(alpha = 0.5, position = 'identity')

# 	m = m + ylab("Occurrences") + xlab("Individuals Accuracies");
# 	m = m + ggtitle(paste(dataname, " dataset accuracies distribution",sep=" "));

#  	print(m);
# 	dev.off();
	
# }

##########################################################################################################
#########################################################################################################