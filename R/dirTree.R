#' create a tree of directories
#'
#' creates a dendogram of subdirectories down to a given depth.
#'
#' @param files whether to look for files. If FALSE, directories are listed.
#' @param depth the depth of the tree
#' @param Dir character, the directory
#' @param exclude vector of regex characters to exclude
#' @param plot whether to plot the tree as well - not used atm
#' @param ... Other args passed to \code{list.dirs} or \code{list.files}
#' @importFrom stringr str_count
#' @importFrom data.tree as.Node ToDataFrameTree
#' @importFrom plyr rbind.fill
#' @family file
#' @export
dirTree <- function(files=FALSE, Dir=".", depth=2, exclude=c("^.git"), plot=FALSE, ...){

  formals(print)$row.names=FALSE

  if (files) {
      ld <- list.files(path = Dir, recursive = TRUE, full.names = FALSE,  ...)
  } else {
      ld <- list.dirs(path = Dir, recursive = TRUE, full.names = FALSE, ...)[-1]
  }


  if (length(ld) == 0)
    stop("No applicable directories", call. = FALSE)

  for (regex in exclude) {
    ld <- ld[!grepl(pattern = regex, x = ld)]
  }

  if (length(ld) == 0)
    stop("No applicable directories", call. = FALSE)

  ld <- ld[sapply(ld, function(x) str_count(x, "/")<=depth)]
  ld <- paste0(basename(Dir),"/",ld)
  x <- lapply(strsplit(ld, "/"), function(z) as.data.frame(t(z)))
  x <- rbind.fill(x)
  x$pathString <- apply(x, 1, function(x) paste(trimws(na.omit(x)), collapse="/"))
  theTree <- as.Node(x)
  df <- ToDataFrameTree(theTree, format = TRUE)
  print(df, row.names=FALSE)

}
