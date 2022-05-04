saveWorkspace <- function(workdir = outDir) {
  
  if (exists("workdir")) {
    save.image(
      paste0(outDir, format(Sys.time(), "%y_%m_%d_workspace.RData"))
    )
    
    writeLines(
      text = capture.output(sessionInfo()), 
      con = paste0(outDir, format(Sys.time(), "%y_%m_%d_sessionInfo.txt"))
    )
  }
  
  if (!exists("workdir")) {
    save.image(
      paste0(format(Sys.time(), "%y_%m_%d_workspace.RData"))
    )
    writeLines(
      text = capture.output(sessionInfo()), 
      con = paste0(format(Sys.time(), "%y_%m_%d_sessionInfo.txt"))
    )
  }
  
}
