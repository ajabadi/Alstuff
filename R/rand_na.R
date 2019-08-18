#' Random NA inseration
#'
#' @param mat a matrix, or data.frame
#' @param prop proportion of NAs
#'
#' @return matrix with NAs
#' @export
#'
#' @examples
#' rand_na()
rand_na <- function(mat=matrix(1:100, ncol = 20), prop=0.3){
  set.seed(42)
  ## calculate the total number of NAs
  total_na <- floor(prop*prod(dim(mat)))
  vec <- as.vector(mat)
  vec[sample(seq_along(vec), size = total_na, replace = FALSE)] <- NA
  matrix(vec, ncol = ncol(mat), dimnames = dimnames(mat))
  ## because it should be
}
