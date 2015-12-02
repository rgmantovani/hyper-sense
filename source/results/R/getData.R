
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

getProblemData = function(problem) {

  files = list.files(path = problem)
  aux = lapply(files, function(file){
     data = dget(paste0(problem, "/", file))
    return(data)
  })
  return(aux)
}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

getSolutionsPerSampleSize = function(data){

  aux = lapply(SCHEDULE, function(sch){
   id.rows = which(data[,"Sampling"] == sch)
    return(data[id.rows, ])
  })
  return(aux)
}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------