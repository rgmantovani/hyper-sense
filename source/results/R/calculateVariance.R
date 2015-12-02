# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

calculateMeanDistances = function(df) {

  SCHEDULE = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)

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
