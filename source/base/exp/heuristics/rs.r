################################################################################################
################################################################################################

#Random Search - Algorithm 

#def search(search_space, max_iter)
#  best = nil
#  max_iter.times do |iter|
#    candidate = {}
#    candidate[:vector] = random_vector(search_space)
#    candidate[:cost] = objective_function(candidate[:vector])
#    best = candidate if best.nil? or candidate[:cost] < best[:cost]
#    puts " > iteration=#{(iter+1)}, best=#{best[:cost]}"
#  end
#  return best
#end

################################################################################################
################################################################################################


#Returns an Instance of Population
rd.create.population = function(n.params, pop.size){

  population = list(chrom=matrix(0, pop.size, n.params), 
    fitness=vector('numeric',pop.size),
    best = 0, changed = FALSE); 

  return (population);
}

################################################################################################
################################################################################################

#Returns a randomized population
rd.random.population = function(population, n.params, limits){

	pop.size = nrow(population$chrom)
	for (i in 1:n.params) {
		population$chrom[,i] = runif(pop.size, limits[i,1], limits[i,2]);
	}

	return(population);
}

################################################################################################
################################################################################################

#Calculates fitness value for each individual
rd.calc.fitness = function(data, population, hyper.space){

	pop.size = nrow(population$chrom)
	tmp = vector('numeric', pop.size);

	for(i in 1:pop.size){   
		params = population$chrom[i,];
		tmp[i] = fitness.fn(params, data, hyper.space);
	}
	return(tmp)
}

################################################################################################
################################################################################################

# parar de executar o AG caso o melhor indivíduo não melhore depois de N gerações
# Stop condition of GA
stop.condition = function(all.individuals, generation, best){

  	if (generation > MAX.ITERATIONS){#} || 
     #stagnation.criteria(all.individuals, generation, best)){
    	return (TRUE);
    }else{
    	return (FALSE);
    }
}


################################################################################################
################################################################################################

#Stop criteria - nothing changed in the last "20" generations
stagnation.criteria = function(all.individuals, generation, best){

	if(length(all.individuals) <= NOT.CHANGE){
   		return (FALSE);
 	}else
 	{
      	idx = seq(generation-1,(generation - NOT.CHANGE));
	   	aux = c();
	 
	   	for(i in idx){
	     	aux = rbind(aux, all.individuals[[i]]$changed);
	   	}
   		
   		#retorna TRUE - se nao houve mudanca do melhor nas ultimas 20 iteracoes
   		return(!any(aux));
 	}
}


################################################################################################
################################################################################################

# Random Search Main Function:
#   - folds: dataset partitions
#   - dimensions: parameters range 

rs.svm = function(folds, hyper.space){

    n = hyper.space$n.dimension;
    dimensions = hyper.space$hyp.range;
    iteration = 1;
    max.iter = MAX.ITERATIONS+1;
    all.individuals = list();
    best.global = NULL;
    best.global$fitness = -1;
    POPULATION <<- c();
    
    start.time = proc.time();

    #Repetir ate que condicao seja satisfeita
    while(!stop.condition(all.individuals, iteration, best.global$fitness)) {

        # criar array de individuos aleatorios
        pop = rd.create.population(n, POP.SIZE);
        pop = rd.random.population(pop, n, dimensions);

        # calcular o fitness dos individuos
        pop$fitness = rd.calc.fitness(folds, pop, hyper.space);
        pop$best = max(pop$fitness);

        # Atualizar o otimo global (best global)
        if(iteration == 1 || (pop$best > best.global$fitness)){
            best.global$fitness = pop$best;
            idx = which.max(pop$fitness);
            best.global$parameters = pop$chrom[idx,];
            pop$changed = TRUE;
        }

        all.individuals[[iteration]] = pop;
        iteration = iteration + 1;
    }

    end.time = proc.time();

    # Encontrar o indice do melhor individuo dentro da populacao total
    best = round(best.global$parameters,ROUNDING);
    # best[3:n] = round(best[3:n]);
    best[3:n] = 0;

    POPULATION = round(POPULATION, ROUNDING);
    subpop = POPULATION[, 1:n];

    #finding the best individual in the complete trace
    ret = which(apply(subpop, 1, function(x) all(x %in% best)))
    index = ret[1];

    # Computar o numero de solucoes geradas
    valid.fit  = round(best.global$fitness, ROUNDING);
    n.solutions = length(which(POPULATION[ ,n+1] == valid.fit));
    cpu.time = (end.time - start.time)[3];

    # Retornar as medidas da tecnica de otimizacao, e a populacao avaliada
    mth = NULL;
    mth$measures = c(POPULATION[index, ], index, n.solutions, MAX.ITERATIONS, 
        MAX.ITERATIONS * POP.SIZE, cpu.time);
    names(mth$measures) = hyper.space$measures;

    mth$population = POPULATION;
    colnames(mth$population) = hyper.space$pop.names;
 
    return(mth);
}

################################################################################################
################################################################################################
