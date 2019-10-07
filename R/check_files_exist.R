#' check input/ouput file/directories are valid
#'
#'
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title input/ouput checker
#' @param io_list list of input/output files/directories
#' @return if any files/directories are missing, will produce an error mentioning the missing ones
#' @export
#' @keywords file check
#' @examples
#' \dontrun{check_files-exist(list('~/.Rprofile'))}
###### You can have a family of function to see in see also
#' @family file
check_files_exist <- function(io_list){
  ## flatten the list
  io_flat <- as.list(unlist(io_list, recursive=TRUE))
  non_exist <- io_flat  %>% .[!unlist(lapply(., file.exists))]
  if (length(non_exist))
    stop(paste0('could not find the following file(s): \n',
                paste0(non_exist, collapse = " \n")))
}
