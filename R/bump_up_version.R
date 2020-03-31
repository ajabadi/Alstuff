#' Bump up z in x.y.z for version number
#'
#' From line starting with \code{Version: } in DESCRIPTION file in the desired
#' branch, this function extracts the version number and increments it by one.
#' @param dir Character, the package directory
#' @param branch Character, the name of branch to bump up
#'
#' @return Updates the version number in \code{Version: x.y.z} line by one
#' @export
#' @family gitflow
bump_up_version <- function(dir = '.', branches = c('devel')) {
  owd <- getwd()
  setwd(dir = dir)
  is_git <- any(grepl(pattern = '^./.git$', x = list.dirs()))
  if (!is_git)
    stop(sprintf("%s is not a git repository", dir))
  if (current_branch() %in% branches) {

    if (any(grepl(pattern = '+Version: [0-9].+[0-9]+.[0-9]+', x = system('git diff HEAD', intern = TRUE)))) {
      message("Version number has been already changed")
      return(NULL)
    }
    version_line <- get_package_version(dir = dir)
    currVersion <- version_line$currVersion
    versionLine<- version_line$versionLine

    splitVersion <- strsplit(currVersion, ".", fixed=TRUE)[[1]]
    nVer <- length(splitVersion)
    currEndVersion <- as.integer(splitVersion[nVer])
    newEndVersion <- as.character(currEndVersion + 1)
    splitVersion[nVer] <- newEndVersion
    newVersion <- paste(splitVersion, collapse=".")

    replacement_cmd <- sprintf("sed -e '%ss/.*/Version: %s/' -i '' DESCRIPTION", versionLine, newVersion)
    system(replacement_cmd)
  } else {
    cat ("no automatic version bump on this branch")
  }
  setwd(owd)

}
