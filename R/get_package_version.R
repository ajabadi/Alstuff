#' Get package version from DESCRIPTION's Version line
#'
#' @param dir Package directory
#'
#' @return Package's version as character x.y.z
#' @export
#' @family gitflow
get_pkg_version <- function(dir = '.', branch = NULL) {
    setwd(dir)

    if (!is.null(branch)) {
        ## stash changes & unstash upon exit after checkout to original branch
        system(sprintf('git add .;git stash push -m "prestash"; git checkout %s', branch))
        on.exit(system(sprintf('git checkout -; git stash pop stash@\\{0\\}')))
    }

    versionLine <- system("awk '/Version/{ print NR; exit }' DESCRIPTION", intern = TRUE)
    versionLine <- as.integer(versionLine)
    currVersion <- read.dcf("DESCRIPTION", fields = "Version")
    currVersion <- as.character(currVersion[1, "Version"])

    list(currVersion = currVersion, versionLine = versionLine)
}
