#' Visualise a colour palette
#'
#' @param cols character vector of colours
#'
#' @return a plot
#' @export
#'
#' @examples
#' plot_col_palette(c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF",
#' "#1F9E89FF", "#35B779FF", "#6DCD59FF", "#B4DE2CFF", "#FDE725FF"
#' ))
plot_col_palette <- function(cols) {
  n <- length(cols)
  plot(x=seq_len(n), y = rep(0, n),
       col = cols, pch=19, cex=2, xlab = '', ylab = '', axes = FALSE)
}
