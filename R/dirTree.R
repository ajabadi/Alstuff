#' create a tree of directories
#'
#' creates a dendogram of subdirectories down to a given depth. Plus a tree plot.
#'
#'@param path the base directory
#'@param depth the depth of the tree
#'@param plot whether to plot the tree as well - not used atm
#'@importFrom stringr str_count
#'@importFrom data.tree as.Node
#'@importFrom plyr rbind.fill
#'@family file
#'@export dirTree

dirTree <- function(Dir, depth=2, plot=FALSE){
  formals(print)$row.names=FALSE
  ld <- list.dirs(path = Dir, recursive = TRUE, full.names = FALSE)[-1]
  ld <- ld[sapply(ld, function(x) str_count(x, "/")<=depth)]
  ld <- paste0(basename(Dir),"/",ld)
  x <- lapply(strsplit(ld, "/"), function(z) as.data.frame(t(z)))
  x <- rbind.fill(x)
  x$pathString <- apply(x, 1, function(x) paste(trimws(na.omit(x)), collapse="/"))
  (theTree <- as.Node(x))

}
