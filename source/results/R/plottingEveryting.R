# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

plottingEverything = function(df, dir.name) {

  #plotar os heatmaps para cada solução de schedule
  SCHEDULE = c(50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)

  aux = lapply(SCHEDULE, function(sch){

    id.rows = which(df[,"Sampling"] == sch)
    dataset = df[id.rows, ]
    heatmap.plot(dataset = dataset, prefix = paste0(dir.name, "/", sch, "-Sampling-"))

  })

}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
