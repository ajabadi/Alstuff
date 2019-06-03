#' magrittr operator addins for RStudio
#'
#' Go to Tools > 'Modify Keybaord Shortcuts' toadd custom shortcuts after installing it.
#'
#' @references \url{https://rstudio.github.io/rstudioaddins/}
#' @importFrom rstudioapi insertText
#' @rdname Addins
#'@export
insertInAddin <- function() {
  insertText(" %in% ")
}

#'@export
insert2wayPipe <- function() {
  insertText(" %<>%  .[")
}
