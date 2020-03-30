#' Get package version from DESCRIPTION's Version line
#'
#' @param dir Package directory
#'
#' @return Package's version as character x.y.z
#' @export
#' @family gitflow
get_pkg_version <- function(dir = '.') {
    setwd(dir)
    versionLine <- system("awk '/Version/{ print NR; exit }' DESCRIPTION", intern = TRUE)
    versionLine <- as.integer(versionLine)
    currVersion <- read.dcf("DESCRIPTION", fields = "Version")
    currVersion <- as.character(currVersion[1, "Version"])

    list(currVersion = currVersion, versionLine = versionLine)
}
