#' Allow loading different versions of a package
#'
#' @param which Character, on of c('new', 'old')
#' @param pkg_name Character, name of the package
#' @param new_path Character, path to new (devel) package
#' @param old_path Character, path to old (reference) installation
#'
#' @return NULL
#'
#' @export
load_pkg_versions <- function(which = c('new', 'old'),
                             pkg_name = 'mixOmics',
                             new_path = '/Users/alabadi/Projects/dev/R/_work/mixOmics/mixOmics_ajabadi/mixOmics_ajabadi',
                             old_path = '/Library/Frameworks/R.framework/Versions/4.0/Resources/library')
{
  which <- match.arg(which)
  if (which == 'new')
  {
    ## cd to new path and report the git branch
    cat("loading new from branch: ",
        system(sprintf("cd %s; git rev-parse --abbrev-ref HEAD", new_path),
               intern = TRUE), '\n')
    suppressMessages(devtools::load_all(new_path))
  } else
  {
    ## if old required
    cat("loading version: ", as.character(packageVersion(pkg_name)))
    suppressMessages(library(pkg_name, character.only = TRUE, lib.loc = old_path))
  }
  invisible(TRUE)
}

