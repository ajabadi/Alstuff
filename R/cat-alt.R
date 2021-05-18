#' testthat style cat
#'
#' @param fmt a sprintf style format
#' @param ... arguments matching the number of \%s in format
#' @import testthat
#' @return none
#'
#' @examples
#' cat2("this is a '%s' text", 'yellow')
#' cat3("this is a '%s' text", 'purple')
#' cat4("this is a '%s' text", 'blue')
#' @name cat-alt
NULL
#' @export
cat2 <- function(fmt, ...)
{ # a yellow cat
  suppressMessages(cat(testthat:::colourise(text = sprintf(fmt, ...), 'error')))
}

#' @export
cat3 <- function(fmt, ...)
{ # a purple cat
  suppressMessages(cat(testthat:::colourise(text = sprintf(fmt, ...), 'warning')))
}

#' @export
cat4 <- function(fmt, ...)
{ # a blue cat
  suppressMessages(cat(testthat:::colourise(text = sprintf(fmt, ...), 'skip')))
}
