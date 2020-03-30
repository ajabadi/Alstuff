#' source all R files on a github repo
#'
#' @param repo of form "user/repo"
#'
#' @importFrom httr GET stop_for_status content
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
