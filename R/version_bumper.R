#' Bump up z in x.y.z for version number
#'
#' @param branch Name of branch to bump up
#' @param dir Package directory
#'
#' @return Updates the version number in \code{Version: x.y.z} line by one
#' @export
#' @family gitflow
version_bumper <- function(branch = 'devel', dir = '.') {
  on_target_branch <- TRUE
  if (!is.na(branch)) {
    on_target_branch <- system('git rev-parse --abbrev-ref HEAD', intern = TRUE) == branch
  }
  if (on_target_branch) {
    version_line <- get_pkg_version(dir = dir)
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

}
