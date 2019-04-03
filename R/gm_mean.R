#' Geometric Mean
#'
#' Calculate the geometric mean of a numeric vector
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title geometric mean
#' @param x numeric vevtor
#' @return wgeometric mean of the vector
#' @export gm_mean
#' @keywords geometric mean
#' @example
#' gm_mean(c(1,1,125))
#' ## 5
#' @family mean

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
