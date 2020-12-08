#' Impute missing values om columns by means/median
#'
#' For type = 'mean' por 'median', the corresponding values for columns
#' are returned. For type = 'knn', \code{impute::knn} is used.
#' @param x A numeric matrix - features are columns
#' @param type One of c('mean', 'median', 'knn')
#' @param ... arguments passed to \code{impute::knn}
#'
#' @return Imputed x
#'
#' @examples
#' impute_missing_values()
#' impute_missing_values(matrix(c(1,2,3, NA,
#'                          4,5,6, NA),
#'                          byrow = FALSE, ncol=2))
#' @export
impute_missing_values <- function(x = matrix(c(1:19,NA), ncol = 4),
                                  type = c('mean', 'median', 'knn'),
                                  ...) {
  requireNamespace('impute')
  if (!is.matrix(x)) {
    stop("'x' must be a numeric matrix")
  }

  type <- match.arg(type)
  ind.na <- which(is.na(x), arr.ind = TRUE)

  if (type == 'mean')
  {
    replacements <- colMeans(x, na.rm = TRUE)
  } else if (type == 'median')
  {
    replacements <- apply(x, 2, median)
  } else if (type == 'knn')
  {
    return(t(impute::impute.knn(t(x), ...)$data))
  }

  x[ind.na] <- replacements[ind.na[,'col']]

  return(x)
}
