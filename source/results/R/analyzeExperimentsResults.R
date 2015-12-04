# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

analyzeExperimentsResults = function() {

  problems = list.dirs( path = "data", recursive = FALSE)

  for(problem in problems) {
  
    problem.name = sub(".*/", "", problem)
    cat("* Problem:", problem, "\n")
    
    dir.name = paste0(getwd(), "/output/", problem.name)
    if(!dir.exists(dir.name)) {
      dir.create(dir.name, recursive = TRUE)
    }

    obj = analyzeProblem(problem = problem, dir.name = dir.name)
    
    # Saving results
    save(x = obj, file = paste0(dir.name, "/all-results.RData"))
    write.csv(x = obj$df, file = paste0(dir.name, "/all-solutions.csv"))
    write.csv(x = obj$mean.distances, file = paste0(dir.name, "/general-measures.csv"))
    write.csv(x = obj$intern.distances$rs.mat, file = paste0(dir.name, "/rs-intern-distances.csv"))
    write.csv(x = obj$intern.distances$pso.mat, file = paste0(dir.name, "/pso-intern-distances.csv"))
     
  }

}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

analyzeProblem = function(problem, dir.name) {

  temp = getProblemData(problem)
  df = do.call("rbind", temp)
  
  mean.distances = calculateMeanDistances(df = df)
  cat("   - Mean distances between solutions ... \n")
 
  intern.distances = calculateInterDistances(df = df)
  cat("   - Mean distances inter the same technique ... \n")

  plottingEverything(temp = temp, dir.name)
  cat("   - Heatmap plots ... \n")

  obj = list(df = df, mean.distances = mean.distances, 
    intern.distances = intern.distances)
  return(obj)
}


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

