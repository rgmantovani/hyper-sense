# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

calculateMeanDistances = function(df) {

  aux = lapply(SCHEDULE, function(sch){

    id.rows = which(df[,"Sampling"] == sch)
    data = df[id.rows, ]

    pso.sol = data[ ,2:3]
    rs.sol  = data[ ,15:16] 
    df.sol  = data[ ,28:29] 

    pso.df.distance = distanceBetweenSolutions(vector1 = pso.sol, vector2 = df.sol)
    rs.df.distance  = distanceBetweenSolutions(vector1 = rs.sol,  vector2 = df.sol)
    pso.rs.distance = distanceBetweenSolutions(vector1 = pso.sol, vector2 = rs.sol)

    indexes = c(1,6:11, 19:24, 32:37)

    temp = colMeans(data)
    obj = c(temp[indexes], pso.df.distance, rs.df.distance, pso.rs.distance)
    return (obj)
  })

  df = do.call("rbind", aux)
  colnames(df)[20] = "Mean.Distance.PSO.DF"
  colnames(df)[21] = "Mean.Distance.RS.DF"
  colnames(df)[22] = "Mean.Distance.PSO.RS"

  return(df)
}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

calculateInterDistances = function(df) {

  pso.sol = df[ ,1:3]
  rs.sol  = df[ ,c(1,15:16)] 
 
  pso.list = getSolutionsPerSampleSize(pso.sol)
  rs.list = getSolutionsPerSampleSize(rs.sol)

  pso.mat = internDistancesBetweenSolutions(pso.list)
  rs.mat = internDistancesBetweenSolutions(rs.list)  

  obj = list(pso.mat = pso.mat, rs.mat = rs.mat)
  return(obj)

}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

distanceBetweenSolutions = function( vector1, vector2) {

  n = nrow(vector1)
  aux = lapply(1:n, function(i){
    obj = rdist(t(vector1[i,]), t(vector2[i,]))
  })

  mean.distance = mean(unlist(aux))
  return(mean.distance)
}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

internDistancesBetweenSolutions = function(technique.list) {

  n = length(technique.list)
  mat = matrix(0, n, n)

  for(i in 1:(n-1)){
    for(j in (i+1):n){
      vector1 = technique.list[[i]]
      vector2 = technique.list[[j]]
      mat[i, j] = distanceBetweenSolutions(vector1 = vector1, vector2 = vector2)
    }
  }

  colnames(mat) = SCHEDULE
  rownames(mat) = SCHEDULE
  return(mat)
}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
