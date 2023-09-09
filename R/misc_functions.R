

dir_init <- function(path, verbose=FALSE, overwrite=TRUE){
  if (path == "/") stop("what are you doing! you cannot format root")
  if (substr(path, 1, 1) == "/") stop("path argument cannot start from root")
  contents <- dir(path, recursive=TRUE)
  if(dir.exists(path)){
    if(overwrite){
      if(verbose){
        if(length(contents)==0) print(paste("folder ", path, " created.", sep=""))
        if(length(contents)>0) print(paste("folder ", path, " wiped of ", length(contents), " files/folders.", sep=""))
      }
      if(dir.exists(path)) unlink(path, recursive=TRUE)
      dir.create(path)
    }
  } else {
    if(verbose){
      print(paste("folder ", path, " created.", sep=""))
    }
    dir.create(path)
  }
}
