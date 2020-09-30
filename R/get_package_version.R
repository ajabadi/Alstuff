#' Get package version and date info from DESCRIPTION
#'
#' @param dir Character, the package directory
#' @param branch Character, the branch name
#'
#' @return Package's version as character x.y.z and date line (if any)
#' @export get_package_version_and_date
#' @family git
get_package_version_and_date <- function(dir = '.', branch = NULL) {
    owd <- getwd()
    setwd(dir)
    if (!is.null(branch)) {
        branch_is_valid(branch)
        if (must_switch_branch(branch)) {
            ## stash changes & unstash upon exit after checkout to original branch
            system("git add .")
            ## uncommitted changes
            if (length(system("git diff HEAD")) > 0) {
                system(sprintf('git stash push -m "prestash"; git checkout %s', branch))
            }

            on.exit(system(sprintf('git checkout -; git stash pop stash@\\{0\\}')))

        }

    }

    versionLine <- system("awk '/Version/{ print NR; exit }' DESCRIPTION", intern = TRUE)
    versionLine <- as.integer(versionLine)
    currVersion <- read.dcf("DESCRIPTION", fields = "Version")
    currVersion <- as.character(currVersion[1, "Version"])

    has_Date <- FALSE
    dateLine <- system("awk '/Date/{ print NR; exit }' DESCRIPTION", intern = TRUE)
    dateLine <- as.integer(dateLine)
    if (length(dateLine) != 0)
    {
        has_Date <- TRUE
    }

    setwd(owd)
    list(currVersion = currVersion, versionLine = versionLine, has_Date = has_Date, dateLine = dateLine)
}

current_branch <- function()
    system('git rev-parse --abbrev-ref HEAD', intern = TRUE)

must_switch_branch <- function(branch = 'master') {
    branch_is_valid(branch)
    if ( current_branch() == branch )
        FALSE
    else
        TRUE
}

branch_is_valid <- function(branch) {
    if (!is.null(branch)) {
        valid_branches <- system("git branch", intern = TRUE)
        require(stringr)
        valid_branches <- stringr::str_extract(valid_branches, pattern = "[a-zA-Z_]{1}[a-zA-Z0-9-_]+")
        if ( !is(branch, "character") )
            stop("'branch' must be character")
        if (! branch %in% valid_branches )
            stop(sprintf("'branch' must be one of: %s", paste(valid_branches, collapse = ', ')))
    }
    TRUE
}

