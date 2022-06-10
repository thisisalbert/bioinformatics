saveWorkspace <- function(workdir = outDir, saveEnv = TRUE, saveInfo = TRUE) {
  
  if (exists("workdir") == TRUE) {
    if (saveEnv) {
      save.image(
        paste0(workdir, format(Sys.time(), "%y_%m_%d_workspace.RData"))
      )
    }
    if (saveInfo) {
      writeLines(
        text = capture.output(sessionInfo()), 
        con = paste0(workdir, format(Sys.time(), "%y_%m_%d_sessionInfo.txt"))
      )
    }
  }
  
  if(exists("workdir") == FALSE) {
    if (saveEnv) {
      save.image(
        paste0(format(Sys.time(), "%y_%m_%d_workspace.RData"))
      )
    }
    if (saveInfo) {
      writeLines(
        text = capture.output(sessionInfo()), 
        con = paste0(format(Sys.time(), "%y_%m_%d_sessionInfo.txt"))
      )
    }
  }
  
  message("Workspace has been saved")
  
}
