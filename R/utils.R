get_os <- function() {
  system_info <- Sys.info()
  if (!is.null(system_info)) {
    os <- system_info["sysname"]
    if (os == "Darwin") 
      os <- "osx"
  }
  else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) 
      os <- "osx"
    if (grepl("linux-gnu", R.version$os)) 
      os <- "linux"
  }
  tolower(os)
}
