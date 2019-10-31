#' Update all packages
#'
#' @return NULL
#' @export
#'
#' @examples
#' \deontrun{BiocUpdateAll()}
#' 
BiocUpdateAll <- function() {
    options("install.packages.compile.from.source" = "no")
    BiocManager::install(update = TRUE, ask=FALSE, site_repository = BiocManager::repositories()[1])
}
