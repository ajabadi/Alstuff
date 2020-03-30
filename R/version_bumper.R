#' Bump up z in x.y.z for version number
#'
#' @param branch Name of branch to bump up
#' @param dir Package directory
#'
#' @return Updates the version number in \code{Version: x.y.z} line by one
#' @export
version_bumper <- function(branch = 'devel', dir = '.') {
  setwd(dir)
  on_target_branch <- TRUE
  if (!is.na(branch)) {
    on_target_branch <- system('git rev-parse --abbrev-ref HEAD', intern = TRUE) == branch
  }
  if (on_target_branch) {
    version_line <- system("awk '/Version/{ print NR; exit }' DESCRIPTION", intern = TRUE)
    version_line <- as.integer(version_line)
    currVersion <- read.dcf("DESCRIPTION", fields = "Version")
    splitVersion <- strsplit(currVersion, ".", fixed=TRUE)[[1]]
    nVer <- length(splitVersion)
    currEndVersion <- as.integer(splitVersion[nVer])
    newEndVersion <- as.character(currEndVersion + 1)
    splitVersion[nVer] <- newEndVersion
    newVersion <- paste(splitVersion, collapse=".")

    replacement_cmd <- sprintf("sed -e '%ss/.*/Version: %s/' -i '' DESCRIPTION", version_line, newVersion)
    system(replacement_cmd)
  } else {
    cat ("no automatic version bump on this branch")
  }

}
