#' Install local package with dependencies
#'
#' Install dependencies based oon the DESCRIPTION file and then installs the
#' package.
#'
#' @param path path to package
#'
#' @return none
#' @export
#'
install_local_package <- function(path='.')
{
  if (!requireNamespace('remotes', quietly = TRUE))
    install.packages('remotes')
  remotes::install_deps(pkgdir = path, dependencies = TRUE, repos = BiocManager::repositories(), Ncpus=2)
  remotes::install_local(path = path, dependencies = FALSE, upgrade = FALSE)
}
