# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

heatmap.plot = function(dataset, prefix){


 	pso.sol = as.data.frame(dataset[ ,2:3])
 	colnames(pso.sol) = c("cost", "gamma")
  pso.sol$technique = "PSO"
 	rs.sol  = as.data.frame(dataset[ ,15:16]) 
  colnames(rs.sol) = c("cost", "gamma")
  rs.sol$technique = "RS"
  df.sol  = as.data.frame(dataset[ ,28:29]) 
  colnames(df.sol) = c("cost", "gamma")
  df.sol$technique = "Default"


  full = rbind(pso.sol, rs.sol, df.sol)
	filename = paste0(prefix, "Heatmap.pdf")
	
	#TODO: mudar para eps
	pdf(filename) #, height=5, width=)

	g = NULL
	g = ggplot(full, aes(x=cost, y=gamma, colour=technique)) 
	g = g + geom_point(aes(shape=as.factor(15), size = 6)) 
	g = g + scale_y_continuous(limits=c(-15, 3))
	g = g + scale_x_continuous(limits=c(-2, 15))
	g = g + ggtitle("SVM hyper-parameters space")
	g = g + scale_shape(guide = 'none')
	g = g + scale_size(guide = 'none')
	print(g)
	dev.off()

}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------


# improvement.plot = function(data){

# 	# 1 - 23
# 	filename = "improvement-1.pdf"
# 	pdf(filename, height=4.5, width=10)

# 	df1 = melt(data[1:23,])
# 	colnames(df1)[2] = "technique"
# 	g = ggplot(df1, aes(x=dataset, y=value, fill=technique, group=technique, width=.75))
#    	g = g + geom_bar(position="dodge",stat="identity")
   	
#    	mP = c("#000000", "#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")
# 	g = g + scale_fill_manual(values=mP)
# 	g = g + ylab("Test Accuracy") + xlab("Data set")
# 	g = g + theme(axis.text.x = element_text(angle = 40, hjust = 1))
# 	print(g)
# 	dev.off()

# 	# 24 - 47
# 	filename = "improvement-2.pdf"
# 	pdf(filename, height=4.5, width=10)

# 	df2 = melt(data[24:47,])
# 	colnames(df2)[2] = "technique"
# 	g = ggplot(df2, aes(x=dataset, y=value, fill=technique, group=technique, width=.75))
#    	g = g + geom_bar(position="dodge",stat="identity")
   	
#    	mP = c("#000000", "#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")
# 	g = g + scale_fill_manual(values=mP)
# 	g = g + ylab("Test Accuracy") + xlab("Data set")
# 	g = g + theme(axis.text.x = element_text(angle = 40, hjust = 1))
# 	print(g)
# 	dev.off()

# 	# 48 - 70
# 	filename = "improvement-3.pdf"
# 	pdf(filename, height=4.5, width=10)

# 	df3 = melt(data[48:70,])
# 	colnames(df3)[2] = "technique"
# 	g = ggplot(df3, aes(x=dataset, y=value, fill=technique, group=technique, width=.75))
#    	g = g + geom_bar(position="dodge",stat="identity")
   	
#    	mP = c("#000000", "#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")
# 	g = g + scale_fill_manual(values=mP)
# 	g = g + ylab("Test Accuracy") + xlab("Data set")
# 	g = g + theme(axis.text.x = element_text(angle = 40, hjust = 1))
# 	print(g)
# 	dev.off()

# }


##########################################################################################################
##########################################################################################################

#Plot das geracoes em que foram encontrados os melhores individuos

# generations.plot = function(data){

# 	df = melt(data, id.vars=c("max", "min", "mean"))
	
# 	limits = aes(ymax = max, ymin=min)
# 	dodge = position_dodge(width=0.9)

# 	filename = "bests-generations.pdf"
# 	pdf(filename, height=4.5, width=10)

# 	g = ggplot(df, aes(x=value, y=mean)) # width=.75))
# 	g = g + geom_bar( stat="identity", fill="#999999", colour="black", position=dodge)
# 	g = g + geom_text(aes(label=mean), size=3, vjust=-0.7, hjust=1.2)
# 	g = g + guides(fill=FALSE)
# 	g = g + ylab("Generation") + xlab("Data set")
# 	g = g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 	g = g + geom_errorbar(limits, position=dodge, width=0.25) # linetype="dotted")
# 	g = g + geom_hline(yintercept=25, linetype="dotted", colour="red")

# 	print(g)
# 	dev.off()


# }

##########################################################################################################
##########################################################################################################

# Histograma da acuracia dos individuos

# ind.histogram = function(data, dataname){

# 	all = do.call("rbind", do.call("rbind", data))

# 	filename = paste(dataname, "acc-distribution.pdf",sep="")
# 	pdf(filename, height=5)
# 	m = NULL

# 	df.test = melt(all[,4])
# 	df.val = melt(all[,3])	

# 	df.test$accuracy = "test"
# 	df.val$accuracy = "validation"

# 	vegLengths = rbind(df.test, df.val)

# 	m = ggplot(vegLengths, aes(x=value, fill = accuracy)) + geom_histogram(alpha = 0.5, position = 'identity')

# 	m = m + ylab("Occurrences") + xlab("Individuals Accuracies")
# 	m = m + ggtitle(paste(dataname, " dataset accuracies distribution",sep=" "))

#  	print(m)
# 	dev.off()
	
# }

##########################################################################################################
#########################################################################################################