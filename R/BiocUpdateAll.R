#' Update all packages
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{BiocUpdateAll()}
#'
BiocUpdateAll <- function() {
    options("install.packages.compile.from.source" = "no")
    BiocManager::install(update = TRUE, ask=FALSE, site_repository = BiocManager::repositories()[1])
}
