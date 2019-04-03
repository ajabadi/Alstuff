#' Break a full file name to parent directory, name, and extension
#'
#' Option to get the file base name without extensions (e.g. .txt)
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title details of the file name
#' @param filePath Full name of the file
#' @importFrom tools file_ext
#' @return A list of file name details
#' @export
#' @keywords basename extension
#' @examples
#' \dontrun{parent_base_ext("directory/file.extension")}
#'
#' @family basename

parent_base_ext <- function(filePath){

  file_dir <- dirname(filePath)
  file_name <- sub("^([^.]*).*", "\\1",basename(filePath))
  file_ext<- tools::file_ext(filePath)
  return(list(parent=file_dir, base=file_name, ext=file_ext))
}
