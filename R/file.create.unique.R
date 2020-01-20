#' base::file.create which ensures exisiting files are indexed and not overwritten
#'
#' @param filePath Character, path to the file
#'
#' @return Full path to the file to be created.
#' @export
#' @examples
#' \dontrun{file.create.unique('~/.Rprofile')}
#'
file.create.unique <- function(filePath) {
  file.base <- parent_base_ext(filePath)$base
  all.files <- list.files(dirname(filePath))
  i <- 1
  original.name <- filePath
  while (filePath %in% all.files) {
    cat(sprintf("%s exists.", filePath))
    filePath <- gsub(x = original.name, pattern = file.base, replacement = sprintf("%s-%s", file.base, i))
    cat(sprintf("Trying to create %s \n", filePath))
    i <- i + 1
  }
  succ <- file.create(filePath)

  if (succ) {
    cat(sprintf("%s was successfully created", filePath))
  }
  return(filePath)
}
