####### This file cannot be tested in usual way as it performs dir and file creation/deletion
#' Back up a file in external hard drive
#'
#' Backs up in either 1../__backup folder or
#' 2. in anotehr (external hard drive) folder path identical to that on local computer or
#' 3. in another (external hard drive) folder
#' @author Al J Abadi, \email{aljalabadi@gmail.com}
#' @title backup files - usually data files before being overwritten
#' @param file The file to be recorded - relative or full path - assumes it's a full path only if it starts with '/'.
#' @param where Backup scheme: 1. NA: In ./backup folder of working directory.
#' 2. 'path': Full path to the folder where a copy of the file and its log will be stored.
#' @param date_ext Subset of \code{c("year","month","day", "hour", "minute")} - details of the renamed file
#' @param run_spec Specification of the run whose output will replace the current one.
#' @param toplevel toplevel directory to replace with \code{where}. NA by default.
#' @return backs up a renamed version of the file and logs the record.
#' @export

file_backup <- function(file,
                        where='/Volumes/0414861919/Mac_backup',
                        date_ext=c("year","month","day"),
                        run_spec= "Description of the latest changes you made",
                        toplevel = NA # '/Users/alabadi'
                        ){

  ## backup directory
  backup_dir <- R.utils::filePath(dirname(file),'__backup')
  if(!is.na(where) & is.na(toplevel)){
    backup_dir <- where
  } else if(!is.na(where) & !is.na(toplevel)){
    backup_dir <- R.utils::filePath(getwd(), backup_dir)
    backup_dir <- gsub(pattern = toplevel, replacement = where, x = backup_dir)
  }

  if(!dir.exists(backup_dir)){
    dir.create(backup_dir, recursive = TRUE)
  }

  ## backup name
  if(substring(file,0,1)!="/"){
    file <- R.utils::filePath(getwd(), file)
  }
  lst <- parent_base_ext(file)
  suff <- c(year="%y", month="%m", day="%d", hour="%H", minute="%M", second="%S")
  suffix <- paste(suff[date_ext], collapse = "_")
  backup_name <- paste0(lst$base,format(Sys.time(), paste0("_",suffix)),'.', lst$ext)
  full_backup_name <- R.utils::filePath(backup_dir, backup_name)

  ## log file
  log_file <- paste0(basename(file),'.log')
  log_file <- R.utils::filePath(backup_dir, log_file)
  if(!file.exists(log_file )){
    file.create(log_file)
    cat(paste0(format(Sys.time(), "%a %b %d %X %Y"), " initialising backup for: \n ",
               file , "\n \n"), file = log_file, append = TRUE)
  }
  ## record in log
  cat(paste0(format(Sys.time(), "%a %b %d %X %Y"), "\n  Run Spec: ",run_spec,".  File: ",full_backup_name , "\n \n \n"), file = log_file, append = TRUE)
  ## back up
  file.copy(file, full_backup_name)
}
