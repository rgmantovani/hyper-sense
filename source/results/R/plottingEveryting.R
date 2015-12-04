# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

plottingEverything = function(temp, dir.name) {

  # Heatmap of the Solutions
  n = length(temp)
  for(i in 1:n){
    
    heatmap.plot(
      dataset = temp[[i]], 
      prefix = paste0(dir.name, "/Run-", i, "-BestSolutions-"),
      colours = c("black", "seagreen2", "purple4")
    )
  
  }

  # Heatmap of the Schedules
  df = do.call("rbind", temp)
  aux = lapply(SCHEDULE, function(sch){

    id.rows = which(df[,"Sampling"] == sch)
    dataset = df[id.rows, ]
    
    heatmap.plot(
      dataset = dataset, 
      prefix = paste0(dir.name, "/", sch, "-Sampling-"),
      colours = c("black", "red", "seagreen2")
    )

  })

}
# colours = c("white", "seagreen2", "springgreen4", "plum3", 
  # "purple4", "blue1", "navyblue"); 
    

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
