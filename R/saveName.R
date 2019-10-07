#' Name for output file
#'
#' @param dir directory
#' @param basename basename befor specs, for example: PCA-plot
#' @param args named arguments list that changes for different outputs
#'
#' @return a string
#' @export saveName
#'
#' @examples
#' saveName(dir='./img', basename='ggplot-of-data',
#'          args=list(alpha=5, beta=3), ext='pdf', spec='gamma-fixed')
saveName <- function(dir='./img', basename, args, ext='pdf', spec=NULL){
  ## ensure dir does not have trailing slash
  gsub('/$','', dir)
  ## ensure extension does not have a redundant dot
  gsub('^.','', ext)
  if(!is.null(spec)) args$spec <- spec
  sprintf('%s/%s_%s.%s',dir,basename,paste0(names(args),'-',args, collapse = '_'),ext)
}
