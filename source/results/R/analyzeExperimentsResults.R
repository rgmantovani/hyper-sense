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
    
  }

}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

analyzeProblem = function(problem, dir.name) {

  files = list.files(path = problem)

  aux = lapply(files, function(file){
    obj = getData(problem, file)
    return(obj)
  })

  df = do.call("rbind", aux)
  
  mean.distances = calculateMeanDistances(df = df)
  cat("   - Mean distances between solutions ... \n")
 
  plottingEverything(df = df, dir.name)
  cat("   - Heatmap plots ... \n")

  obj = list(df = df, mean.distances = mean.distances)
  return(obj)
}


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

