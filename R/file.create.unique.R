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
  all.files.base <- list.files(dirname(filePath))
  original.file.full <- filePath
  file.name <- tools::file_path_sans_ext(basename(filePath))

  final.file.full <- filePath
  final.file.base <- basename(filePath)
  i <- 1
  while (final.file.base %in% all.files.base) {
    cat(sprintf("%s already exists.", final.file.full))
    final.file.full <- gsub(x = original.file.full, pattern = file.name, replacement = sprintf("%s-%s", file.name, i))
    final.file.base <- basename(final.file.full)
    cat(sprintf("Trying to create %s ... \n", final.file.full))
    i <- i + 1
  }
  succ <- file.create(final.file.full)

  if (succ) {
    cat(sprintf("%s was successfully created\n", final.file.full))
  }
  return(final.file.full)
}
