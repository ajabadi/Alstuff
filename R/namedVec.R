#' Create a named vector
#'
#' @param names names
#' @param vec vector
#'
#' @return a named vector
#' @export
#'
#' @examples
#'  nemedVec(names=c('girl', 'boy'), vec=('sara', 'jack'))
namedVec <- function(names, vec){
  names(vec) <- names
  vec
}
