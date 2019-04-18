#' Histogram with cut colors
#'
#' Partition histogram by color - output is a ggplot2 object which can be extended using \code{labs} etc
#' @author Al J Abadi, \email{aljalabadi@@gmail.com}
#' @title Histogram color partitioner
#' @param vec a numeric vector
#' @param cutoff numeric valu(s) of cut points (atomic or vector)
#' @param cols vector of colors to use, if NULL, \code{gg_color_hue(length(cutoff)+1)} will be used
#' @param legend_title title of the legend
#' @param legend_labels character vector of label of the legends
#' @param n_bins number of bins
#' @param ... other parameters passed to \code{ggplot}
#' @import ggplot2
#' @return a histogram with different colors b/w cutoff points
## if you want this function to be accessible outside the package:: (what if there are multiple function here?)
#' @export
#' @keywords histogram
#' @examples hist_cut_color() + labs(x='X label here', title='Main title here')
#' @family hist

hist_cut_color <- function(vec=rnorm(2000, mean = 0, sd = 10),
                           cutoff = c(-10,-3,3, 10),
                           cols=c('red', 'green', 'orange','yellow','purple'),
                           legend_title='QC',
                           legend_labels = c('Filtered', 'Passed', 'orange','yellow','purple'),
                           n_bins=200,...){
  if(is.null(cols)){
    cols <- gg_color_hue(length(cutoff)+1)
  }
  # hist <- hist(x, plot=FALSE,...)
  # cuts <- cut(hist$breaks, dput(c(-Inf,cutoff,Inf)))
  # plot(hist, col=cols[cuts],...)
  n_cols <- length(cutoff)+1
  df <- data.frame(x=vec, cuts=cut(vec, dput(c(-Inf,cutoff,Inf))))
  ggplot(df, aes(vec, fill=cuts)) + geom_histogram(bins=n_bins) + theme_bw() +
    scale_fill_manual(guide_legend(legend_title), labels = legend_labels, values=cols[1:n_cols]) +
    labs(...)
}
