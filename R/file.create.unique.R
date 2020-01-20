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
  file.base <- basename(filePath)
  all.files.base <- list.files(dirname(filePath))
  i <- 1
  original.file.full <- filePath
  while (file.base %in% all.files.base) {
    cat(sprintf("%s already exists.", filePath))
    file.name <- tools::file_path_sans_ext(basename(filePath))
    filePath <- gsub(x = original.file.full, pattern = file.name, replacement = sprintf("%s-%s", file.base, i))
    file.base <- basename(filePath)
    cat(sprintf("Trying to create %s ... \n", filePath))
    i <- i + 1
  }
  succ <- file.create(filePath)

  if (succ) {
    cat(sprintf("%s was successfully created\n", filePath))
  }
  return(filePath)
}
