#' base::file.create which ensures exisiting files are indexed
#'
#' @param filePath Character, path to the file
#'
#' @return NULL
#' @export
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
  file.create(filePath)
}
