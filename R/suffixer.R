#'Add suffix to file name - even with full path
#'
#' Add a suffix to a file name. Could be full path.
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @param file full/relative path to the file name
#' @param suffix descriptional suffix to add
#' @return file name modified by suffix
#' @family file
#' @export
suffixer <- function(file, suffix){
  pl <- parent_base_ext(file) ## path list
  pl$sfx <- suffix
  with(pl, sprintf('%s/%s%s.%s', parent, base,sfx,ext))
}
