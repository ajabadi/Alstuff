#' Update the package from github
#'
#' @param force Logical, force re-install?
#'
#' @export
#' @rdname install_utils
#' @family install_utils
update_Altools <- function(force = TRUE){
  devtools::install_github("ajabadi/Altools", force = force, upgrade = FALSE)
}
## ------------------------------------------------------------------------ ##

#' install.package compile source option
#'
#' @param src one of c("both", "yes", "no")
#' @export
#' @rdname install_utils
#' @family utils
compile_from_source <- function(src = c("both", "yes", "no")) {
  src <- match.arg(src, several.ok = FALSE)
  options("install.packages.compile.from.source" = src)
}
## ------------------------------------------------------------------------ ##

#' Update all packages
#'
#' @return NULL
#' @export
#'
#' @rdname install_utils
#' @family install_utils
#' @examples
#' \dontrun{BiocUpdateAll()}
#'
BiocUpdateAll <- function() {
  options("install.packages.compile.from.source" = "no")
  BiocManager::install(update = TRUE, ask=FALSE, site_repository = BiocManager::repositories()[1])
}
## ------------------------------------------------------------------------ ##

#' source all R files on a github repo
#'
#' @param repo of form "user/repo"
#'
#' @importFrom httr GET stop_for_status content
#' @rdname utils
#' @return sources all R files
#' @export
source_from_github <- function(repo="ajabadi/Altools", files = NULL) {
  src_rep <- sprintf("https://github.com/%s/blob/master", repo)
  req <- GET(sprintf("https://api.github.com/repos/%s/git/trees/master?recursive=1", repo))
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  Rfiles <- grep(pattern = "^R/[a-zA-Z0-9._\\-]*.R$", x = filelist, value = TRUE)

  if (!is.null(files)) {
    Rfiles <- sapply(files, function(x) grep(pattern = x, x = Rfiles, value = TRUE) )
  }
  Rfiles_raw <- file.path(src_rep, Rfiles, "?raw=TRUE")
  sapply(Rfiles_raw, devtools::source_url)
}

## ------------------------------------------------------------------------ ##
#' character formatted with colour
#'
#' Can only be used with the \code{cat} function
#'
#' @param char character
#'
#' @return None
#' @export
#'
#' @examples
#' cat(sprintf("haha! this is %s", char_pretty('cool')))
char_pretty <- function(char) {
  sprintf("\033[33m'%s'\033[39m", char)
}
