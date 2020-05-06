#' Random NA inseration
#'
#' @param mat Matrix, or data.frame of data
#' @param prop Numeric b/w 0 and 1, proportion of NAs
#' @param seed Integer, seed number
#'
#' @return matrix with NAs
#' @export
#'
#' @examples
#' rand_na()
rand_na <- function(mat=matrix(1:100, ncol = 20), prop=0.3, seed = NULL){

  if (!is.null(seed))
  {
    RNGversion('3.6.5')
    set.seed(seed)
  }
  ## calculate the total number of NAs
  total_na <- floor(prop*prod(dim(mat)))
  vec <- as.vector(mat)
  vec[sample(seq_along(vec), size = total_na, replace = FALSE)] <- NA
  matrix(vec, ncol = ncol(mat), dimnames = dimnames(mat))
  ## because it should be
}
