#' Update the package from github
#'
#' @param force Logical, force re-install?
#'
#' @export
update_Altools <- function(force = TRUE){
  devtools::install_github("ajabadi/Altools", force = force, upgrade = FALSE)
}
