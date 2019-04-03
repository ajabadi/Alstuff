#' Geometric Mean
#'
#' Calculate the geometric mean of a numeric vector
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title geometric mean
#' @param x numeric vevtor
#' @return wgeometric mean of the vector
#' @export gm_mean
#' @keywords geometric mean
#' @examples
#' \dontrun{gm_mean(c(1,1,125))}
#' @family mean

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
