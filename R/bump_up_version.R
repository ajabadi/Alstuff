#' Bump up z in x.y.z for version number and update the Date
#'
#' From line starting with \code{Version: } in DESCRIPTION file in the desired
#' branch, this function extracts the version number and increments it by one.
#' It also writes to 'Date:' entry using today's date, if any.
#' @param dir Character, the package directory where DESCRIPTION is located
#' @param branch Character, the name of branch to bump up
#'
#' @return None
#' @export
#' @family git
bump_up_version <- function(dir = '.', branches = c('devel')) {
  owd <- getwd()
  setwd(dir = dir)
  is_git <- any(grepl(pattern = '^./.git$', x = list.dirs()))
  if (!is_git)
    stop(sprintf("%s is not a git repository", dir))
  if (current_branch() %in% branches) {

    pkg_info <- get_package_version_and_date(dir = dir)

    if (any(grepl(pattern = '\\+Version: [0-9].+[0-9]+.[0-9]+', x = system('git diff HEAD', intern = TRUE)))) {
      message("Version number has been already changed")
    }
    else
    {
      currVersion <- pkg_info$currVersion
      versionLine<- pkg_info$versionLine

      splitVersion <- strsplit(currVersion, ".", fixed=TRUE)[[1]]
      nVer <- length(splitVersion)
      currEndVersion <- as.integer(splitVersion[nVer])
      newEndVersion <- as.character(currEndVersion + 1)
      splitVersion[nVer] <- newEndVersion
      newVersion <- paste(splitVersion, collapse=".")

      version_replacement_cmd <- sprintf("sed -e '%ss/.*/Version: %s/' -i '' DESCRIPTION", versionLine, newVersion)
      system(version_replacement_cmd)

    }

    if (pkg_info$has_Date)
    {
      date_replacement_cmd <- sprintf("sed -e '%ss/.*/Date: %s/' -i '' DESCRIPTION", pkg_info$dateLine, as.Date(Sys.Date()))
      system(date_replacement_cmd)
    }

  } else {
    cat ("no automatic version/date bump on this branch")
  }
  setwd(owd)
  NULL
}
