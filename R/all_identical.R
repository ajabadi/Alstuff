#' Check if all list elements are identical
#'
#' @param lst A list
#'
#' @return Logical, indicating if all elemenets are identical
#' @export
#'
#' @examples
#' all_identical(list(a=1, b=1, c=1, d=2))
all_identical <- function(lst) {
  if (is.null(names(lst))) {
    names(lst) <- seq_along(lst)
  }
  for (j in seq_along(lst[-1])) {
    if (!identical(lst[[j]], lst[[j+1]]))
      stop(sprintf("not identical elements: %s", paste(names(lst)[c(j, j+1)], collapse = " & ")), call. = FALSE)
  }
  TRUE
}
