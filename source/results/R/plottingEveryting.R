# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

plottingEverything = function(df, dir.name) {

  aux = lapply(SCHEDULE, function(sch){

    id.rows = which(df[,"Sampling"] == sch)
    dataset = df[id.rows, ]
    heatmap.plot(dataset = dataset, prefix = paste0(dir.name, "/", sch, "-Sampling-"))

  })

}

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
