#' magrittr operator addins for RStudio
#'
#' Go to Tools > 'Modify Keybaord Shortcuts' toadd custom shortcuts after installing it.
#'
#'@references \url{https://rstudio.github.io/rstudioaddins/}
#'@importFrom rstudioapi insertText
#'@family misc

#'@rdname Addins
#'@usage {(Add to RStudio keyboard shortcuts)}
#'@export
insertInAddin <- function() {
  insertText(" %in% ")
}


#'@importFrom rstudioapi insertText
#'@rdname Addins
#'@usage {(Add to RStudio keyboard shortcuts)}
#'@export

insert2wayPipe <- function() {
  insertText(" %<>% .[")
}


#'@importFrom rstudioapi insertText
#'@rdname Addins
#'@usage {(Add to RStudio keyboard shortcuts)}
#'@export
plus <- function(){
  insertText(" + ")
}
