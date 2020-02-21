#' Handle default NULL values
#'
#' @param null_arg function arg that is set to NULL
#' @param default_value default value
#'
#' @return default value if the arg is NULL, otherwise the arg value
#' @export
#'
#' @examples
#' set_if_null(NULL, 10)
#' #> 10
#' set_if_null(5, 10)
#' #> 5
set_if_null <- function(null_arg=NULL, default_value) {
  return(
    if (is.null(null_arg))
      default_value
    else
      null_arg
  )
}
