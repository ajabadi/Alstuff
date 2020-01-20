#' Update the package from github
#'
#' @export
update_Altools <- function(){
  devtools::install_github("ajabadi/Altools", force = TRUE, upgrade = FALSE)
}
