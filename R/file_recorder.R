####### This file cannot be tested in usual way as it performs dir and file creation/deletion
#' Create a record of the overwritten file
#'
#' Backs up a file in \code{logFolder} before being overwriting it and logs the record
#' @author Al J Abadi, \email{aljalabadi@gmail.com}
#' @title file recorder
#' @param outFile The file to be recorded e.g. ~/Documents/mydata.Rds - full path from the working directory
#' @param logFolder Full path to the record-keeping folder - full path relative to \code{outFile}. e.g. ./_log
#' @param logFile Name of the log file to use for records, e.g. record.log. if \code{NULL}, \code{paste0(logFolder,"/",outFile,'.log')} will be used.
#' @param date_ext Subset of \code{c("year","month","day", "hour", "hour", "minute")} - details of the renamed file
#' @param run_spec Specification of the run whose output will replace the current one.
#' @return Writes a renamed version of the file to logFolder and records in logFile.
#' @import tools
#' @export
#' @keywords recorder log backup overwrite file_recorder
#' @examples
#' ex_file <- '~/example.txt'
#' ex_dir <- './file_recorder_tmp'
#' file.create('~/example.txt')
#' file_recorder(outFile = ex_file, logFolder = ex_dir,
#'               logFile = "ex_log", run_spec = "example run with no parameter")
#' ## remove directory/files
#' unlink(ex_dir)
#' file.remove(ex_file)
###### You can have a family of function to see in see also
#' @family check_files_exist
###### You can also include examples as full path to an R file in the project directory
## #' @example ./relative/path/to/example.R

file_recorder <- function(outFile, ## full path to the file to be recorded
                          logFolder="__log",
                          logFile=NULL,
                          date_ext=c("year","month","day", "hour"),
                          run_spec= "New run"){
  ## gets full outFile path in outFile_dir, renames, logs and and moves it to outFile_dir/logFolder with run_spec

  ## INPUT out_file: full file name
  ## INPUT log_file: a writable text file
  ## OUTPUT NULL: just renaming the file, if it exists, or a message that no file is being overwritten

  ## get the file dir, name, and ext
  file_dir <- dirname(outFile)
  file_name <- sub("^([^.]*).*", "\\1",basename(outFile))
  file_ext<- tools::file_ext(outFile)
  ## create the full log folder path
  log_folder_full <- file.path(file_dir, logFolder)

  ## log directory exists
  if(!dir.exists(log_folder_full)){
    dir.create(log_folder_full)
  }

  if(is.null(logFile)){ ## default log file name of not supplied
    logFile <- paste0(basename(outFile),".log")
  } else {
    logFile <- paste0(logFile,".log")
  }

  ## full log file path
  log_file_full <- file.path(log_folder_full, logFile)
  ## create it if it does not exist
  if(!file.exists(log_folder_full)){
    file.create(log_file_full)
  }
  ## record the run spec
  cat(paste0(format(Sys.time(), "%a %b %d %X %Y"), "\n  Run Spec: ",run_spec,"\n"), file = log_file_full, append = TRUE)

  if(file.exists(outFile)){
    suffix <- c(year="%y", month="%m", day="%d", hour="%H", minute="%M", second="%S")
    suff <- paste(suffix[date_ext], collapse = "_")
    ## put the date b/w file name and extension
    record_full_name <- file.path(log_folder_full,paste0(file_name, format(Sys.time(), paste0("_overwritten_",suff)), ".",file_ext))
    file.copy(outFile, record_full_name)
    cat(paste0(" Overwritten file recorded as: ", record_full_name,"\n \n"), file = log_file_full, append = TRUE)
    message(paste0(outFile," renamed to: ", record_full_name))
  } else {
    message("No file overwritten - supplied run spec stored in log file")
  }
}
