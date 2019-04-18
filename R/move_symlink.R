#' Move a file/folder and symlink
#'
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title file transferer
#' @param path full path to the file/folder (~ not allowed)
#' @return copies the files folder in a replicate directory in external and symlinks
#' @export

move_symlink <-  function(path='/Users/alabadi/Downloads/trash', external='/Volumes/0414861919/Mac_Home', home='/Users/alabadi'){

  if(!dir.exists(external)) stop('invalid external directory')
  if(!dir.exists(home)) stop('invalid home directory')
  
  ext_path <- gsub(home, external, path)

################## FOLDER
if(dir.exists(path)){
  ## create the external folderand to levels if they do not exist
  if(!dir.exists(ext_path)){
    if(!dir.create(ext_path, recursive = TRUE)) stop('External directory could not be created')
  }
  ## copy the full path preserving the dates
  file.copy(from=path,to = dirname(ext_path), recursive = TRUE, copy.date = TRUE)
  ## remove original
  unlink(path, recursive = TRUE)
  ## symlink
  file.symlink(ext_path, path)
  
  ################## FILE
} else if(file.exists(path)){
  ## ensure the directory exists
  if(!dir.exists(dirname(ext_path))){
    if(!dir.create(dirname(ext_path), recursive = TRUE)) stop('External directory could not be created')
  }
  ## copy
  if(!file.copy(from=path,to = ext_path, copy.date = TRUE)) stop('File could not be transferred')
  ## remove original
  file.remove(path)
  ## create symlink
  file.symlink(ext_path, path)
  ################## NONE
} else {
  stop(cat('No such file or directory: ', path))
}

}