#' Check input files exist
#'
#'
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title File checker
#' @param file_list vector of full paths to files
#' @return if any files are missing, will produce an error mentioning the files
#' @export
#' @keywords file check
#' @examples
#' \dontrun{check_files-exist('~/.Rprofile')}
###### You can have a family of function to see in see also
#' @family file_recorder
check_files_exist = function(input_list){

  ##INPUT file_list: list of full file names (including directory)
  non_exist <- input_list %>% .[!unlist(lapply(., file.exists))]
  if(length(non_exist)) stop (paste0('could not find the following file(s): \n', non_exist))
}
