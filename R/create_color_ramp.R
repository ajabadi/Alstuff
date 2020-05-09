#' Create and visualise a color ramp
#'
#' @param col_range Colors to transition from/to
#' @param n Number of colors to create
#'
#' @return A visual of colors and a character vector of colors
#' @export
#'
#' @examples
#' create_color_ramp(c('#a4eaff', '#0a5090'))
create_color_ramp <- function(col_range = c("red","yellow","springgreen","royalblue"), n = 30) {

  cols <- colorRampPalette(col_range)(n)
  plot(x=seq_len(n), y = sin(seq_len(n)*pi*((length(col_range)-1))/n),
       col = cols, pch=19, cex=2, xlab = '', ylab = '', axes = FALSE)

  return(cols)
}
