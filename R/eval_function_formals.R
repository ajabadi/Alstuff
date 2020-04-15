#' Evaluate function formals in an environment
#'
#' @param fun Name, function name
#' @param envir environment, in  which to evaluate. (.GlobalEnv by default)
#' @param ... passed to base::eval
#'
#' @return Function formals are evaluated in the specified environment
#' @export
#' @family utils
#' @examples
#' eval_function_formals(rnorm)
eval_function_formals <- function(fun, envir = .GlobalEnv, ...) {
  fun_formals <- formals(fun)
  fun_formals <- formals(fun)
  fun_formals$... <- NULL ## if any
  invisible(lapply(X = names(fun_formals), FUN = function(x){
    cat(sprintf("evaluating %s ...", x))
    tryCatch({
      assign(x = x, value = eval(fun_formals[[x]]), envir = envir, ...)
      cat(sprintf("successful\n", x))
      },
             error = function(e) message(sprintf("\nNo default value for '%s'", x)))
  }))
}
