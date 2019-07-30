#' Short description not ending in a full stop
#'
#' A detailed description
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title this thing doer
#' @param param1 description of param1 and so on for others
#' @return what it return
#' @import pkg_1_to_be_loaded pkg_2
#' @importFrom pkg_3 specific_function_from_pkg_3
## if you want this function to be accessible outside the package:: (what if there are multiple function here?)
#' @export foo
###### If you like to leave a reference that helps, or inspired/helped you in writing this
#' @references \url{https://google.com}
#' @keywords keyword1 keyword2
#' @example
#' bar(3)
###### You can have a family of function to see in see also
#' @family file
###### You can also include examples as full path to an R file in the project directory
## #' @example ./relative/path/to/example.R

bar <- function(param2){(return(paste0(param2, "_NEW")))}

foo <- function(param1, fun=bar){paste("bar of", param1," is: ", bar(param1))}
