#' Get package version from DESCRIPTION's Version line
#'
#' @param dir Character, the package directory
#' @param branch Character, the branch name
#'
#' @return Package's version as character x.y.z
#' @export get_package_version
#' @family gitflow
get_package_version <- function(dir = '.', branch = NULL) {
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
    setwd(owd)
    list(currVersion = currVersion, versionLine = versionLine)
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

